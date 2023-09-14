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
                channelParamGen = (_, _) => UserChannelParams(Seq.fill(15) { UserVirtualChannelParams(4) }),
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

class AnySmallRocketMeshConfig(nX: Int, nY: Int) extends Config(
    new freechips.rocketchip.subsystem.WithNSmallCores(n = nX * nY - 3) ++
    new freechips.rocketchip.subsystem.WithNBanks(n = 1) ++
    new WithMesh2DNoC(nX, nY)
)

class AnySmallRocketButterflyConfig(kAry: Int, nFly: Int) extends Config(
    new freechips.rocketchip.subsystem.WithNSmallCores(
            n = scala.math.pow(kAry, nFly).toInt) ++
    new freechips.rocketchip.subsystem.WithNBanks(n = 1) ++
    new WithButterflyNoC(kAry, nFly)
)



class SmallRocket1CoreBusConfig extends AnySmallRocketBusConfig(1)
class SmallRocket2CoreBusConfig extends AnySmallRocketBusConfig(2)
class SmallRocket4CoreBusConfig extends AnySmallRocketBusConfig(4)
class SmallRocket8CoreBusConfig extends AnySmallRocketBusConfig(8)
class SmallRocket16CoreBusConfig extends AnySmallRocketBusConfig(16)
class SmallRocket32CoreBusConfig extends AnySmallRocketBusConfig(32)
class SmallRocket64CoreBusConfig extends AnySmallRocketBusConfig(64)
class SmallRocket128CoreBusConfig extends AnySmallRocketBusConfig(128)




class SmallRocket1CoreRingConfig extends AnySmallRocketRingConfig(1)
class SmallRocket2CoreRingConfig extends AnySmallRocketRingConfig(2)
class SmallRocket4CoreRingConfig extends AnySmallRocketRingConfig(4)
class SmallRocket8CoreRingConfig extends AnySmallRocketRingConfig(8)
class SmallRocket16CoreRingConfig extends AnySmallRocketRingConfig(16)
class SmallRocket32CoreRingConfig extends AnySmallRocketRingConfig(32)
class SmallRocket64CoreRingConfig extends AnySmallRocketRingConfig(64)
class SmallROcket128CoreRingConfig extends AnySmallRocketRingConfig(128)


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



class SmallRocket1x1CoreMeshConfig extends AnySmallRocketMeshConfig(1, 1)
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

// class SmallRocket2Ary0FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 0)
// class SmallRocket2Ary1FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 1)

class SmallRocket2Ary2FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 2)
class SmallRocket2Ary3FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 3)
class SmallRocket2Ary4FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 4)
class SmallRocket2Ary5FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 5)
class SmallRocket2Ary6FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 6)
class SmallRocket2Ary7FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 7)
class SmallRocket2Ary8FlyCoreButterflyConfig extends AnySmallRocketButterflyConfig(2, 8)


class GemminiCustomChipConfig[T <: Data : Arithmetic, U <: Data, V <: Data](
    gemminiConfig: GemminiArrayConfig[T, U, V] = GemminiConfigs.chipConfig
) extends Config((site, here, up) => {
    case BuildRoCC => up(BuildRoCC) ++ Seq(
        (p: Parameters) => {
            implicit val q = p
            val gemmini = LazyModule(new Gemmini(gemminiConfig))
            gemmini
        }
    )
})

abstract class AnySmallRocketWithGemmini(n: Int) extends Config (
    new GemminiCustomChipConfig ++
    new freechips.rocketchip.subsystem.WithNSmallCores(n = n) ++
    new MinimalSimulationConfig
)


class SmallRocketWithGemmini1CoreBusConfig extends AnySmallRocketWithGemmini(1)
class SmallRocketWithGemmini2CoreBusConfig extends AnySmallRocketWithGemmini(2)
class SmallRocketWithGemmini4CoreBusConfig extends AnySmallRocketWithGemmini(4)
class SmallRocketWithGemmini8CoreBusConfig extends AnySmallRocketWithGemmini(8)
class SmallRocketWithGemmini16CoreBusConfig extends AnySmallRocketWithGemmini(16)
class SmallRocketWithGemmini32CoreBusConfig extends AnySmallRocketWithGemmini(32)
class SmallRocketWithGemmini64CoreBusConfig extends AnySmallRocketWithGemmini(64)
class SmallRocketWithGemmini128CoreBusConfig extends AnySmallRocketWithGemmini(128)
