package chipyard.example.simulation

import chipyard.example.simulation.SimulationAbstractConfig
import org.chipsalliance.cde.config.{Config, Parameters}

import gemmini.Arithmetic
import gemmini.GemminiArrayConfig
import gemmini.GemminiConfigs
import freechips.rocketchip.tile.BuildRoCC
import gemmini.Gemmini
import freechips.rocketchip.diplomacy.LazyModule
import chisel3.Data
import scala.collection.immutable.ListMap
import constellation.noc.NoCParams
import constellation.channel.UserChannelParams
import constellation.channel.UserVirtualChannelParams
import constellation.routing.UnidirectionalTorus1DDatelineRouting
import constellation.routing.NonblockingVirtualSubnetworksRouting
import constellation.topology.UnidirectionalTorus1D
import constellation.topology.BidirectionalTorus1D
import constellation.routing.BidirectionalTorus1DShortestRouting
import constellation.topology.TerminalRouter
import constellation.topology.BidirectionalTorus2D
import constellation.routing.DimensionOrderedBidirectionalTorus2DDatelineRouting
import constellation.router.UserRouterParams
import constellation.channel.FlowParams
import constellation.topology.Mesh2D
import constellation.routing.TerminalRouterRouting
import constellation.routing.Mesh2DEscapeRouting
import constellation.channel.UserIngressParams
import constellation.channel.UserEgressParams
import constellation.routing.EscapeChannelRouting
import constellation.routing.Mesh2DDimensionOrderedRouting
import constellation.routing.Mesh2DMinimalRouting
import constellation.routing.BlockingVirtualSubnetworksRouting
import constellation.routing.BidirectionalTorus2DDatelineRouting
import constellation.routing.BidirectionalTorus1DDatelineRouting
import constellation.topology.Butterfly
import constellation.routing.ButterflyRouting
import constellation.channel.ChannelBuffer
import gemmini.CapacityInKilobytes
import gemmini.Dataflow
import gemmini.ScaleArguments

class BaseRocketConfig extends Config(
    new freechips.rocketchip.subsystem.WithSynchronousRocketTiles ++
    new SimulationAbstractConfig
)

abstract class AnySmallRocketConfig(nCores: Int, nL2Banks: Int = 2) extends Config (
    new freechips.rocketchip.subsystem.WithNSmallCores(n = nCores) ++
    new freechips.rocketchip.subsystem.WithNBanks(n = nL2Banks) ++
    new BaseRocketConfig
)

class SmallRocket1CoreConfig extends AnySmallRocketConfig(1)

class SmallRocket2CoreConfig extends AnySmallRocketConfig(2)

class SmallRocket4CoreConfig extends AnySmallRocketConfig(4)

class SmallRocket8CoreConfig extends AnySmallRocketConfig(8)

class SmallRocket16CoreConfig extends AnySmallRocketConfig(16)




class MediumRocket1CoreConfig extends Config(
    new freechips.rocketchip.subsystem.WithNMedCores(1) ++
    new SimulationAbstractConfig
)

class MediumRocket4CoreConfig extends Config(
    new freechips.rocketchip.subsystem.WithNMedCores(4) ++
    new SimulationAbstractConfig
)

class BigRocket1CoreConfig extends Config(
    new freechips.rocketchip.subsystem.WithNBigCores(1) ++
    new SimulationAbstractConfig
)

class BigRocket4CoreConfig extends Config(
    new freechips.rocketchip.subsystem.WithNBigCores(4) ++
    new SimulationAbstractConfig
)

class BigRocket8CoreConfig extends Config(
    new freechips.rocketchip.subsystem.WithNBigCores(8) ++
    new SimulationAbstractConfig
)

class BigRocket16CoreConfig extends Config(
    new freechips.rocketchip.subsystem.WithNBigCores(16) ++
    new SimulationAbstractConfig
)




// only works with a single L2 bank
class WithRingConfig(nCores: Int) extends Config(
    new constellation.soc.WithSbusNoC(constellation.protocol.TLNoCParams(
        constellation.protocol.DiplomaticNetworkNodeMapping(
            inNodeMapping =  ListMap.from(List.tabulate(nCores) { i =>
                (s"Core $i " -> i)
            }),
            outNodeMapping = ListMap(
                "system[0]" -> (0 max (nCores)), // L2 bank
                "system[1]" -> (0 max (nCores + 1)), // MMIO
                "pbus" -> (0 max (nCores + 2))
            )
        ),
        constellation.noc.NoCParams(
            topology = TerminalRouter(UnidirectionalTorus1D(nCores + 3)),
            channelParamGen = (a, b) => UserChannelParams(Seq.fill(10) { UserVirtualChannelParams(4) }),
            routingRelation = BlockingVirtualSubnetworksRouting(
                TerminalRouterRouting(BidirectionalTorus1DDatelineRouting()), 5, 2)
        )
    )) ++
    new MinimalSimulationConfig
)

class WithTorus2DConfig(nX: Int, nY: Int) extends Config(
    new constellation.soc.WithSbusNoC(
        constellation.protocol.TLNoCParams(
            nodeMappings = constellation.protocol.DiplomaticNetworkNodeMapping(
                inNodeMapping = ListMap.from(List.tabulate(nX * nY - 3){
                    i => (s"Core $i " -> i) // note the white space after $i, do no remove it
                }),
                outNodeMapping = ListMap(
                    "system[0]" -> (0 max (nX * nY - 1)), // L2 bank
                    "system[1]" -> (0 max (nX * nY - 2)), // MMIO
                    "pbus" -> (0 max (nX * nY - 3))
                )
            ),
            nocParams = constellation.noc.NoCParams(
                topology = TerminalRouter(BidirectionalTorus2D(nX, nY)),
                channelParamGen = (_, _) => UserChannelParams(Seq.fill(10) { UserVirtualChannelParams(4) }),
                routingRelation = BlockingVirtualSubnetworksRouting(
                    TerminalRouterRouting(DimensionOrderedBidirectionalTorus2DDatelineRouting()),
                    5,
                    2
                )
            )
        )
    ) ++
    new MinimalSimulationConfig
)

class WithMesh2DNoC(nX: Int, nY: Int) extends Config(
    new constellation.soc.WithSbusNoC({
        require(nX * nY >= 4, "Mesh is too small!")

        constellation.protocol.TLNoCParams(
           nodeMappings = constellation.protocol.DiplomaticNetworkNodeMapping(
                inNodeMapping = ListMap.from(List.tabulate(nX * nY - 3){
                    i => (s"Core $i " -> i)
                }),
                outNodeMapping = ListMap(
                    "system[0]" -> (0 max (nX * nY - 1)), // Memory
                    "system[1]" -> (0 max (nX * nY - 2)), // MMIO
                    "pbus" -> (0 max (nX * nY - 3))
                )
            ),
            nocParams = constellation.noc.NoCParams(
                topology = TerminalRouter(Mesh2D(nX, nY)),
                channelParamGen = (_, _) => UserChannelParams(
                    virtualChannelParams = Seq.fill(5) {
                        UserVirtualChannelParams(4)
                    },
                    channelGen = (u) => {
                        implicit val p: Parameters = u
                        ChannelBuffer(4) := _
                    },
                    useOutputQueues = true // prevent long comb paths between terminal routers
                ),
                routingRelation = BlockingVirtualSubnetworksRouting(
                    f = TerminalRouterRouting(
                        Mesh2DDimensionOrderedRouting()
                    ),
                    n = 5, // RADDR, RDATA, WADDR, WRESP, WDATA for AXI and A-B-C-D-E for TL
                    nDedicated = 1
                ),
                routerParams = _ => UserRouterParams(
                    payloadBits = 64,
                    combineSAST = false,
                    combineRCVA = false,
                    coupleSAVA = false,
                ),
                skipValidationChecks = false,
            )
        )
    }) ++
    new MinimalSimulationConfig
)

class WithButterflyNoC(kAry: Int, nFly: Int) extends Config ({
    val butterfly = Butterfly(kAry, nFly)
    val nCores = butterfly.nNodes - 3
    new constellation.soc.WithSbusNoC(
        constellation.protocol.TLNoCParams(
           nodeMappings = constellation.protocol.DiplomaticNetworkNodeMapping(
                inNodeMapping = ListMap.from(List.tabulate(nCores){
                    i => (s"Core $i " -> i)
                }),
                outNodeMapping = ListMap(
                    "system[0]" -> (0 max (nCores - 1)), // Memory
                    "system[1]" -> (0 max (nCores - 2)), // MMIO
                    "pbus" -> (0 max (nCores - 3))
                )
            ),
            nocParams = constellation.noc.NoCParams(
                topology = TerminalRouter(butterfly),
                channelParamGen = (_, _) => UserChannelParams(virtualChannelParams = Seq.fill(5) {
                        UserVirtualChannelParams(4)
                    },
                    channelGen = (u) => {
                        implicit val p: Parameters = u
                        ChannelBuffer(4) := _
                    },
                    useOutputQueues = true // prevent long comb paths between terminal routers
                ),
                routingRelation = BlockingVirtualSubnetworksRouting(
                    f = TerminalRouterRouting(
                        ButterflyRouting()
                    ),
                    n = 5, // RADDR, RDATA, WADDR, WRESP, WDATA
                    nDedicated = 3
                ),
                skipValidationChecks = false,
            )
        )
    ) ++
    new MinimalSimulationConfig
})

class AnySmallRocketBusConfig(nCores: Int) extends Config(
    new freechips.rocketchip.subsystem.WithNSmallCores(n = nCores) ++
    new freechips.rocketchip.subsystem.WithNBanks(n = 1) ++
    new MinimalSimulationConfig
)

class AnyBigRocketBusConfig(nCores: Int) extends Config(
    new freechips.rocketchip.subsystem.WithNBigCores(n = nCores) ++
    new freechips.rocketchip.subsystem.WithNBanks(n = 1) ++
    new MinimalSimulationConfig
)

class AnyMediumRocketBusConfig(nCores: Int) extends Config(
    new freechips.rocketchip.subsystem.WithNMedCores(n = nCores) ++
    new freechips.rocketchip.subsystem.WithNBanks(n = 1) ++
    new MinimalSimulationConfig
)


class AnySmallRocketRingConfig(nCores: Int) extends Config(
    new freechips.rocketchip.subsystem.WithNSmallCores(n = nCores) ++
    new freechips.rocketchip.subsystem.WithNBanks(n = 1) ++
    new WithRingConfig(nCores)
)
class AnySmallRocketTorusConfig(nX: Int, nY: Int) extends Config(
    new freechips.rocketchip.subsystem.WithNSmallCores(n = nX * nY - 3) ++
    new freechips.rocketchip.subsystem.WithNBanks(n = 1) ++
    new WithTorus2DConfig(nX, nY)
)

class AnySmallRocketButterflyConfig(kAry: Int, nFly: Int) extends Config(
    new freechips.rocketchip.subsystem.WithNSmallCores(
            n = scala.math.pow(kAry, nFly).toInt - 3) ++
    new freechips.rocketchip.subsystem.WithNBanks(n = 1) ++
    new WithButterflyNoC(kAry, nFly)
)

class AnyRocketMeshConfig(nX: Int, nY: Int) extends Config(
     new freechips.rocketchip.subsystem.WithNBanks(n = 1) ++
    new WithMesh2DNoC(nX, nY)
)

class AnySmallRocketMeshConfig(nX: Int, nY: Int) extends Config(
    new freechips.rocketchip.subsystem.WithNSmallCores(n = nX * nY - 3) ++
    new AnyRocketMeshConfig(nX, nY)
)

class AnyMediumRocketMeshConfig(nX: Int, nY: Int) extends Config(
    new freechips.rocketchip.subsystem.WithNMedCores(n = nX * nY - 3) ++
    new AnyRocketMeshConfig(nX, nY)
)

class AnyBigRocketMeshConfig(nX: Int, nY: Int) extends Config(
    new freechips.rocketchip.subsystem.WithNBigCores(n = nX * nY - 3) ++
    new AnyRocketMeshConfig(nX, nY)
)



object GemminiCustomConfigs {


    val large16x16 =  GemminiConfigs.defaultConfig.copy(
        meshRows = 16,
        meshColumns = 16,
        sp_capacity = CapacityInKilobytes(64),
        acc_capacity = CapacityInKilobytes(32),
        dataflow = Dataflow.WS,
        acc_singleported = true,
        acc_sub_banks = 2,
        mesh_output_delay = 2,
        ex_read_from_acc = false,
        ex_write_to_spad = false,
        hardcode_d_to_garbage_addr = true,
        // disable combinational scaling
        mvin_scale_args = Option.empty[ScaleArguments[chisel3.SInt, gemmini.Float]],
        mvin_scale_acc_args = Option.empty[ScaleArguments[chisel3.SInt, gemmini.Float]]
    )

    val small8x8 = large16x16.copy(
        meshRows = 8, meshColumns = 8,
        ld_queue_length = 4,
        st_queue_length = 2,
        ex_queue_length = 8
    )
}

class GemminiCustomChipConfig[T <: Data : Arithmetic, U <: Data, V <: Data](
    gemminiConfig: GemminiArrayConfig[T, U, V] = GemminiCustomConfigs.large16x16
) extends Config((site, here, up) => {
    case BuildRoCC => up(BuildRoCC) ++ Seq(
        (p: Parameters) => {
            implicit val q = p
            val gemmini = LazyModule(new Gemmini(gemminiConfig))
            gemmini
        }
    )
})



class WithGemminiMeshNoC(nX: Int, nY: Int) extends Config(
    new constellation.soc.WithSbusNoC({
        require(nX * nY >= 4, "Mesh is too small!")

        constellation.protocol.TLNoCParams(
           nodeMappings = constellation.protocol.DiplomaticNetworkNodeMapping(
                inNodeMapping = ListMap.from(List.tabulate(nX * nY - 3){
                    i => (s"Core $i " -> i)
                } ++ List.tabulate(nX * nY - 3) {
                    i => (s"stream-reader[$i],stream-writer[$i]" -> i)
                }),
                outNodeMapping = ListMap(
                    "system[0]" -> (0 max (nX * nY - 1)), // Memory
                    "system[1]" -> (0 max (nX * nY - 2)), // MMIO
                    "pbus" -> (0 max (nX * nY - 3))
                )
            ),
            nocParams = constellation.noc.NoCParams(
                topology = TerminalRouter(Mesh2D(nX, nY)),
                channelParamGen = (_, _) => UserChannelParams(
                    virtualChannelParams = Seq.fill(5) {
                        UserVirtualChannelParams(4)
                    },
                    channelGen = (u) => {
                        implicit val p: Parameters = u
                        ChannelBuffer(4) := _
                    },
                    useOutputQueues = true // prevent long comb paths between terminal routers
                ),
                routingRelation = BlockingVirtualSubnetworksRouting(
                    f = TerminalRouterRouting(
                        Mesh2DDimensionOrderedRouting()
                    ),
                    n = 5, // RADDR, RDATA, WADDR, WRESP, WDATA for AXI and A-B-C-D-E for TL
                    nDedicated = 1
                ),
                routerParams = _ => UserRouterParams(
                    payloadBits = 64,
                    combineSAST = false,
                    combineRCVA = false,
                    coupleSAVA = false,
                ),
                skipValidationChecks = false,
            )
        )
    }) ++
    new MinimalSimulationConfig
)

class AnySmallRocketWithGemminiMeshConfig(nX: Int, nY: Int) extends Config(
    new GemminiCustomChipConfig ++
    new freechips.rocketchip.subsystem.WithNSmallCores(n = nX * nY - 3) ++
    new freechips.rocketchip.subsystem.WithNBanks(n = 1) ++
    new WithGemminiMeshNoC(nX, nY)
)

class AnySmallRocketWithGemminiBusConfig(n: Int) extends Config(
    new GemminiCustomChipConfig(GemminiCustomConfigs.large16x16) ++
    new freechips.rocketchip.subsystem.WithNSmallCores(n = n) ++
    new MinimalSimulationConfig
)

class AnySmallRocketWithSmallGemminiBusConfig(n: Int) extends Config(
    new GemminiCustomChipConfig(GemminiCustomConfigs.small8x8) ++
    new freechips.rocketchip.subsystem.WithNSmallCores(n = n) ++
    new MinimalSimulationConfig
)




class SmallRocket1CoreBusConfig extends AnySmallRocketBusConfig(1)
class SmallRocket2CoreBusConfig extends AnySmallRocketBusConfig(2)
class SmallRocket4CoreBusConfig extends AnySmallRocketBusConfig(4)
class SmallRocket8CoreBusConfig extends AnySmallRocketBusConfig(8)
class SmallRocket16CoreBusConfig extends AnySmallRocketBusConfig(16)
class SmallRocket32CoreBusConfig extends AnySmallRocketBusConfig(32)
class SmallRocket64CoreBusConfig extends AnySmallRocketBusConfig(64)
class SmallRocket128CoreBusConfig extends AnySmallRocketBusConfig(128)

class MediumRocket1CoreBusConfig extends AnyMediumRocketBusConfig(1)
class MediumRocket2CoreBusConfig extends AnyMediumRocketBusConfig(2)
class MediumRocket4CoreBusConfig extends AnyMediumRocketBusConfig(4)
class MediumRocket8CoreBusConfig extends AnyMediumRocketBusConfig(8)
class MediumRocket16CoreBusConfig extends AnyMediumRocketBusConfig(16)
class MediumRocket32CoreBusConfig extends AnyMediumRocketBusConfig(32)
class MediumRocket64CoreBusConfig extends AnyMediumRocketBusConfig(64)
class MediumRocket128CoreBusConfig extends AnyMediumRocketBusConfig(128)




class BigRocket1CoreBusConfig extends AnyBigRocketBusConfig(1)
class BigRocket2CoreBusConfig extends AnyBigRocketBusConfig(2)
class BigRocket4CoreBusConfig extends AnyBigRocketBusConfig(4)
class BigRocket8CoreBusConfig extends AnyBigRocketBusConfig(8)
class BigRocket16CoreBusConfig extends AnyBigRocketBusConfig(16)
class BigRocket32CoreBusConfig extends AnyBigRocketBusConfig(32)
class BigRocket64CoreBusConfig extends AnyBigRocketBusConfig(64)
class BigRocket128CoreBusConfig extends AnyBigRocketBusConfig(128)

class SmallRocket1CoreRingConfig extends AnySmallRocketRingConfig(1)
class SmallRocket2CoreRingConfig extends AnySmallRocketRingConfig(2)
class SmallRocket4CoreRingConfig extends AnySmallRocketRingConfig(4)
class SmallRocket8CoreRingConfig extends AnySmallRocketRingConfig(8)
class SmallRocket16CoreRingConfig extends AnySmallRocketRingConfig(16)
class SmallRocket32CoreRingConfig extends AnySmallRocketRingConfig(32)
class SmallRocket64CoreRingConfig extends AnySmallRocketRingConfig(64)
class SmallRocket128CoreRingConfig extends AnySmallRocketRingConfig(128)


class SmallRocket1x1CoreTorusConfig extends AnySmallRocketTorusConfig(1, 1)
class SmallRocket2x2CoreTorusConfig extends AnySmallRocketTorusConfig(2, 2)
class SmallRocket3x3CoreTorusConfig extends AnySmallRocketTorusConfig(3, 3)
class SmallRocket4x4CoreTorusConfig extends AnySmallRocketTorusConfig(4, 4)
class SmallRocket5x5CoreTorusConfig extends AnySmallRocketTorusConfig(5, 5)
class SmallRocket6x6CoreTorusConfig extends AnySmallRocketTorusConfig(6, 6)
class SmallRocket7x7CoreTorusConfig extends AnySmallRocketTorusConfig(7, 7)
class SmallRocket8x8CoreTorusConfig extends AnySmallRocketTorusConfig(8, 8)
class SmallRocket9x9CoreTorusConfig extends AnySmallRocketTorusConfig(9, 9)
class SmallRocket10x10CoreTorusConfig extends AnySmallRocketTorusConfig(10, 10)
class SmallRocket11x11CoreTorusConfig extends AnySmallRocketTorusConfig(11, 11)
class SmallRocket12x12CoreTorusConfig extends AnySmallRocketTorusConfig(12, 12)




class SmallRocket2x2CoreMeshConfig extends AnySmallRocketMeshConfig(2, 2)
class SmallRocket3x3CoreMeshConfig extends AnySmallRocketMeshConfig(3, 3)
class SmallRocket4x4CoreMeshConfig extends AnySmallRocketMeshConfig(4, 4)
class SmallRocket5x5CoreMeshConfig extends AnySmallRocketMeshConfig(5, 5)
class SmallRocket6x6CoreMeshConfig extends AnySmallRocketMeshConfig(6, 6)
class SmallRocket7x7CoreMeshConfig extends AnySmallRocketMeshConfig(7, 7)
class SmallRocket8x8CoreMeshConfig extends AnySmallRocketMeshConfig(8, 8)
class SmallRocket9x9CoreMeshConfig extends AnySmallRocketMeshConfig(9, 9)
class SmallRocket10x10CoreMeshConfig extends AnySmallRocketMeshConfig(10, 10)
class SmallRocket11x11CoreMeshConfig extends AnySmallRocketMeshConfig(11, 11)
class SmallRocket12x12CoreMeshConfig extends AnySmallRocketMeshConfig(12, 12)
class SmallRocket13x13CoreMeshConfig extends AnySmallRocketMeshConfig(13, 13)
class SmallRocket14x14CoreMeshConfig extends AnySmallRocketMeshConfig(14, 14)
class SmallRocket15x15CoreMeshConfig extends AnySmallRocketMeshConfig(14, 14)
class SmallRocket16x16CoreMeshConfig extends AnySmallRocketMeshConfig(16, 16)
class SmallRocket17x17CoreMeshConfig extends AnySmallRocketMeshConfig(17, 17)
class SmallRocket18x18CoreMeshConfig extends AnySmallRocketMeshConfig(18, 18)
class SmallRocket19x19CoreMeshConfig extends AnySmallRocketMeshConfig(19, 19)
class SmallRocket20x20CoreMeshConfig extends AnySmallRocketMeshConfig(20, 20)


class MediumRocket2x2CoreMeshConfig extends AnyMediumRocketMeshConfig(2, 2)
class MediumRocket3x3CoreMeshConfig extends AnyMediumRocketMeshConfig(3, 3)
class MediumRocket4x4CoreMeshConfig extends AnyMediumRocketMeshConfig(4, 4)
class MediumRocket5x5CoreMeshConfig extends AnyMediumRocketMeshConfig(5, 5)
class MediumRocket6x6CoreMeshConfig extends AnyMediumRocketMeshConfig(6, 6)
class MediumRocket7x7CoreMeshConfig extends AnyMediumRocketMeshConfig(7, 7)
class MediumRocket8x8CoreMeshConfig extends AnyMediumRocketMeshConfig(8, 8)
class MediumRocket9x9CoreMeshConfig extends AnyMediumRocketMeshConfig(9, 9)
class MediumRocket10x10CoreMeshConfig extends AnyMediumRocketMeshConfig(10, 10)
class MediumRocket11x11CoreMeshConfig extends AnyMediumRocketMeshConfig(11, 11)
class MediumRocket12x12CoreMeshConfig extends AnyMediumRocketMeshConfig(12, 12)
class MediumRocket13x13CoreMeshConfig extends AnyMediumRocketMeshConfig(13, 13)
class MediumRocket14x14CoreMeshConfig extends AnyMediumRocketMeshConfig(14, 14)
class MediumRocket15x15CoreMeshConfig extends AnyMediumRocketMeshConfig(14, 14)
class MediumRocket16x16CoreMeshConfig extends AnyMediumRocketMeshConfig(16, 16)
class MediumRocket17x17CoreMeshConfig extends AnyMediumRocketMeshConfig(17, 17)
class MediumRocket18x18CoreMeshConfig extends AnyMediumRocketMeshConfig(18, 18)
class MediumRocket19x19CoreMeshConfig extends AnyMediumRocketMeshConfig(19, 19)
class MediumRocket20x20CoreMeshConfig extends AnyMediumRocketMeshConfig(20, 20)


class BigRocket2x2CoreMeshConfig extends AnyBigRocketMeshConfig(2, 2)
class BigRocket3x3CoreMeshConfig extends AnyBigRocketMeshConfig(3, 3)
class BigRocket4x4CoreMeshConfig extends AnyBigRocketMeshConfig(4, 4)
class BigRocket5x5CoreMeshConfig extends AnyBigRocketMeshConfig(5, 5)
class BigRocket6x6CoreMeshConfig extends AnyBigRocketMeshConfig(6, 6)
class BigRocket7x7CoreMeshConfig extends AnyBigRocketMeshConfig(7, 7)
class BigRocket8x8CoreMeshConfig extends AnyBigRocketMeshConfig(8, 8)
class BigRocket9x9CoreMeshConfig extends AnyBigRocketMeshConfig(9, 9)
class BigRocket10x10CoreMeshConfig extends AnyBigRocketMeshConfig(10, 10)
class BigRocket11x11CoreMeshConfig extends AnyBigRocketMeshConfig(11, 11)
class BigRocket12x12CoreMeshConfig extends AnyBigRocketMeshConfig(12, 12)
class BigRocket13x13CoreMeshConfig extends AnyBigRocketMeshConfig(13, 13)
class BigRocket14x14CoreMeshConfig extends AnyBigRocketMeshConfig(14, 14)
class BigRocket15x15CoreMeshConfig extends AnyBigRocketMeshConfig(14, 14)
class BigRocket16x16CoreMeshConfig extends AnyBigRocketMeshConfig(16, 16)
class BigRocket17x17CoreMeshConfig extends AnyBigRocketMeshConfig(17, 17)
class BigRocket18x18CoreMeshConfig extends AnyBigRocketMeshConfig(18, 18)
class BigRocket19x19CoreMeshConfig extends AnyBigRocketMeshConfig(19, 19)
class BigRocket20x20CoreMeshConfig extends AnyBigRocketMeshConfig(20, 20)



// class SmallRocket2Ary0FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 0)
// class SmallRocket2Ary1FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 1)

class SmallRocket2Ary2FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 2)
class SmallRocket2Ary3FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 3)
class SmallRocket2Ary4FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 4)
class SmallRocket2Ary5FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 5)
class SmallRocket2Ary6FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 6)
class SmallRocket2Ary7FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 7)
class SmallRocket2Ary8FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 8)


class SmallRocketWithGemmini1CoreBusConfig extends AnySmallRocketWithGemminiBusConfig(1)
class SmallRocketWithGemmini2CoreBusConfig extends AnySmallRocketWithGemminiBusConfig(2)
class SmallRocketWithGemmini4CoreBusConfig extends AnySmallRocketWithGemminiBusConfig(4)
class SmallRocketWithGemmini8CoreBusConfig extends AnySmallRocketWithGemminiBusConfig(8)
class SmallRocketWithGemmini16CoreBusConfig extends AnySmallRocketWithGemminiBusConfig(16)
class SmallRocketWithGemmini32CoreBusConfig extends AnySmallRocketWithGemminiBusConfig(32)
class SmallRocketWithGemmini64CoreBusConfig extends AnySmallRocketWithGemminiBusConfig(64)
class SmallRocketWithGemmini128CoreBusConfig extends AnySmallRocketWithGemminiBusConfig(128)


class SmallRocketWithSmallGemmini1CoreBusConfig extends AnySmallRocketWithSmallGemminiBusConfig(1)
class SmallRocketWithSmallGemmini2CoreBusConfig extends AnySmallRocketWithSmallGemminiBusConfig(2)
class SmallRocketWithSmallGemmini4CoreBusConfig extends AnySmallRocketWithSmallGemminiBusConfig(4)
class SmallRocketWithSmallGemmini8CoreBusConfig extends AnySmallRocketWithSmallGemminiBusConfig(8)
class SmallRocketWithSmallGemmini16CoreBusConfig extends AnySmallRocketWithSmallGemminiBusConfig(16)
class SmallRocketWithSmallGemmini32CoreBusConfig extends AnySmallRocketWithSmallGemminiBusConfig(32)
class SmallRocketWithSmallGemmini64CoreBusConfig extends AnySmallRocketWithSmallGemminiBusConfig(64)
class SmallRocketWithSmallGemmini128CoreBusConfig extends AnySmallRocketWithSmallGemminiBusConfig(128)


class SmallRocketWithGemmini2x2CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(2, 2)
class SmallRocketWithGemmini3x3CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(3, 3)
class SmallRocketWithGemmini4x4CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(4, 4)
class SmallRocketWithGemmini5x5CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(5, 5)
class SmallRocketWithGemmini6x6CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(6, 6)
class SmallRocketWithGemmini7x7CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(7, 7)
class SmallRocketWithGemmini8x8CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(8, 8)
class SmallRocketWithGemmini9x9CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(9, 9)
class SmallRocketWithGemmini10x10CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(10, 10)
class SmallRocketWithGemmini11x11CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(11, 11)
class SmallRocketWithGemmini12x12CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(12, 12)
class SmallRocketWithGemmini13x13CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(13, 13)
class SmallRocketWithGemmini14x14CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(14, 14)
class SmallRocketWithGemmini15x15CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(15, 15)
class SmallRocketWithGemmini16x16CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(16, 16)
class SmallRocketWithGemmini17x17CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(17, 17)
class SmallRocketWithGemmini18x18CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(18, 18)
class SmallRocketWithGemmini19x19CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(19, 19)
class SmallRocketWithGemmini20x20CoreMeshConfig extends AnySmallRocketWithGemminiMeshConfig(20, 20)



