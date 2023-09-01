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

abstract class AnySmallRocketWithGemmmini(n: Int) extends Config (
    new GemminiCustomChipConfig ++
    new freechips.rocketchip.subsystem.WithNSmallCores(n = n) ++
    new BaseRocketConfig
)


class SmallRocketWithGemmmini1CoreConfig extends AnySmallRocketWithGemmmini(1)
class SmallRocketWithGemmmini2CoreConfig extends AnySmallRocketWithGemmmini(2)


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
class BaseRocketRingConfig(nCores: Int) extends Config(
    new constellation.soc.WithSbusNoC(constellation.protocol.TLNoCParams(
        constellation.protocol.DiplomaticNetworkNodeMapping(
            inNodeMapping =  ListMap.from(List.tabulate(nCores) { i =>
                println(s"Core $i -> $i")
                (s"Core $i" -> i)
            } :+ ("serial-tl" -> (nCores))),
            outNodeMapping = ListMap(
                "system[0]" -> (nCores + 1), // L2 bank
                "system[1]" -> (nCores + 2), // MMIO
                "pbus" -> nCores
            )
        ),
        constellation.noc.NoCParams(
            topology = TerminalRouter(BidirectionalTorus1D(nCores + 3)),
            channelParamGen = (a, b) => UserChannelParams(Seq.fill(16) { UserVirtualChannelParams(4) }),
            routingRelation = NonblockingVirtualSubnetworksRouting(UnidirectionalTorus1DDatelineRouting(), 16, 1)
        )
    )) ++
    new MinimalSimulationConfig
)

class BaseRocketTorus2DConfig(nX: Int, nY: Int) extends Config(
    new constellation.soc.WithSbusNoC(
        constellation.protocol.TLNoCParams(
            nodeMappings = constellation.protocol.DiplomaticNetworkNodeMapping(
                inNodeMapping = ListMap.from(List.tabulate(nX * nY){
                    i => (s"Core $i " -> i)
                } :+ ("serial-tl" -> (nX * nY))),
                outNodeMapping = ListMap(
                    "system[0]" -> (nX * nY - 1), // L2 bank
                    "system[1]" -> (nX * nY - 1), // MMIO
                    "pbus" -> (nX * nY - 1)
                )
            ),
            nocParams = constellation.noc.NoCParams(
                topology = BidirectionalTorus2D(nX, nY),
                channelParamGen = (_, _) => UserChannelParams(Seq.fill(10) { UserVirtualChannelParams(4) }),
                routingRelation = NonblockingVirtualSubnetworksRouting(DimensionOrderedBidirectionalTorus2DDatelineRouting(), 5, 2)
            )
        )
    ) ++
    new MinimalSimulationConfig
)

// class SbusRingNoCConfig extends Config(
//   new constellation.soc.WithSbusNoC(constellation.protocol.TLNoCParams(
//     constellation.protocol.DiplomaticNetworkNodeMapping(
//       inNodeMapping = ListMap(
//         "Core 0" -> 0,
//         "Core 1" -> 1,
//         "Core 2" -> 2,
//         "Core 3" -> 3,
//         "Core 4" -> 4,
//         "Core 5" -> 5,
//         "Core 6" -> 6,
//         "Core 7" -> 7,
//         "serial-tl" -> 8),
//       outNodeMapping = ListMap(
//         "system[0]" -> 9,  // ExtMem
//         "system[1]" -> 10, // MMIO
//         // "system[2]" -> 11,
//         // "system[3]" -> 12,
//         // "system[4]" -> 13, // MMIO
//         "pbus" -> 8)), // TSI is on the pbus, so serial-tl and pbus should be on the same node
//     NoCParams(
//       topology        = UnidirectionalTorus1D(11),
//       channelParamGen = (a, b) => UserChannelParams(Seq.fill(10) { UserVirtualChannelParams(4) }),
//       routingRelation = NonblockingVirtualSubnetworksRouting(UnidirectionalTorus1DDatelineRouting(), 5, 2))
//   )) ++
//   new freechips.rocketchip.subsystem.WithNBigCores(8) ++
//   new freechips.rocketchip.subsystem.WithNBanks(1) ++
//   new MinimalSimulationConfig
// )

class AnySmallRocketRingConfig(nCores: Int) extends Config(
    new freechips.rocketchip.subsystem.WithNSmallCores(n = nCores) ++
    new freechips.rocketchip.subsystem.WithNBanks(n = 1) ++
    new BaseRocketRingConfig(nCores)
)
class AnySmallRocketTorusConfig(nX: Int, nY: Int) extends Config(
    new freechips.rocketchip.subsystem.WithNSmallCores(n = nX * nY) ++
    new freechips.rocketchip.subsystem.WithNBanks(n = 1) ++
    new BaseRocketTorus2DConfig(nX, nY)
)

class SmallRocket1CoreRingConfig extends AnySmallRocketRingConfig(1)
class SmallRocket2CoreRingConfig extends AnySmallRocketRingConfig(2)
class SmallRocket4CoreRingConfig extends AnySmallRocketRingConfig(4)
class SmallRocket8CoreRingConfig extends AnySmallRocketRingConfig(8)
class SmallRocket16CoreRingConfig extends AnySmallRocketRingConfig(16)
class SmallRocket32CoreRingConfig extends AnySmallRocketRingConfig(32)


class SmallRocket2x2CoreTorusConfig extends AnySmallRocketTorusConfig(2, 2)
class SmallRocket3x3CoreTorusConfig extends AnySmallRocketTorusConfig(3, 3)
class SmallRocket4x4CoreTorusConfig extends AnySmallRocketTorusConfig(4, 4)
class SmallRocket5x5CoreTorusConfig extends AnySmallRocketTorusConfig(5, 5)
class SmallRocket6x6CoreTorusConfig extends AnySmallRocketTorusConfig(6, 6)
// abstract class AnySmallRocketRingConfig(nCores:Int, nBanks: Int = 1) extends Config(
//     new freechips.rocketchip.subsystem.WithNSmallCores(n = nCores) ++
//     new freechips.rocketchip.subsystem.WithNBanks(n = nBanks) ++
//     new testchipip.WithRingSystemBus ++
//     new BaseRocketConfig
// )

// class SmallRocket2CoreRingConfig extends AnySmallRocketRingConfig(2, 1)
// class SmallRocket4CoreRingConfig extends AnySmallRocketRingConfig(4, 2)
// class SmallRocket8CoreRingConfig extends AnySmallRocketRingConfig(8, 4)


