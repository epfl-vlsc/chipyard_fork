package chipyard.example.simulation


import constellation.noc.NoCParams
import constellation.channel.UserChannelParams
import constellation.channel.UserVirtualChannelParams
import constellation.topology.TerminalRouter
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
import org.chipsalliance.cde.config.{Config, Parameters}
import constellation.topology.HierarchicalTopology
import constellation.topology.HierarchicalSubTopology
import constellation.routing.HierarchicalRouting
import scala.collection.immutable.ListMap
import chipyard.example.simulation.MinimalSimulationConfig



class WithMultiMeshNoC(nX: Int, nY: Int, mX: Int, mY: Int) extends Config({

    val topology = TerminalRouter(
            HierarchicalTopology(
                base = Mesh2D(mX, mY),
                children = Seq.tabulate(mX * mY) { s =>
                    HierarchicalSubTopology(s, 0, Mesh2D(nX, nY))
                }
            )
        )
    val routing = BlockingVirtualSubnetworksRouting(
        f = TerminalRouterRouting(
            HierarchicalRouting(
                baseRouting = Mesh2DDimensionOrderedRouting(),
                childRouting = Seq.fill(mX * mY) {
                    Mesh2DDimensionOrderedRouting()
                }
            )
        ),
        n = 5, // RADDR, RDATA, WADDR, WRESP, WDATA for AXI and A-B-C-D-E for TL
        nDedicated = 1
    )
    val nocParams = constellation.noc.NoCParams(
            topology = topology,
            routingRelation = routing,
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
            routerParams = _ => UserRouterParams(
                payloadBits = 64,
                combineSAST = false,
                combineRCVA = false,
                coupleSAVA = false,
            ),
            skipValidationChecks = false, // takes forever to check
        )
    val nSockets = mX * mY
    val coresPerSocket = nX * nY
    val coresTotal = mX * mY * coresPerSocket
    val coreMapping: ListMap[String, Int] = ListMap.from(List.tabulate(nSockets) { s =>
        val offset = nX * nY * s
        List.tabulate(nX * nY) { i =>
            (s"Core ${i + offset} " -> (i + offset + nSockets))
        }
    }.flatten)
    new constellation.soc.WithSbusNoC(
        tlnocParams = constellation.protocol.TLNoCParams(
            nodeMappings = constellation.protocol.DiplomaticNetworkNodeMapping(
                inNodeMapping = coreMapping,
                outNodeMapping = ListMap(
                    "pbus" -> 0,
                    s"system[0]" -> 0, // MEM
                    s"system[1]" -> 0 // MMIO
                )
            ),
            nocParams = nocParams
        )
    ) ++
    new freechips.rocketchip.subsystem.WithInclusiveCache(capacityKB = 64) ++
    new freechips.rocketchip.subsystem.WithNBanks(n = 1) ++
    new MinimalSimulationConfig
})

class AnySmallRocketMultiMeshConfig(nX: Int, nY: Int, mX: Int, mY: Int) extends Config(
    new freechips.rocketchip.subsystem.WithNSmallCores(n = 2 * nX * nY) ++
    new WithMultiMeshNoC(nX, nY, mX, mY)
)





class SmallRocket2x2Core2x1MeshConfig extends AnySmallRocketMultiMeshConfig(2, 2, 2, 1)
class SmallRocket3x3Core2x1MeshConfig extends AnySmallRocketMultiMeshConfig(3, 3, 2, 1)
class SmallRocket4x4Core2x1MeshConfig extends AnySmallRocketMultiMeshConfig(4, 4, 2, 1)
class SmallRocket5x5Core2x1MeshConfig extends AnySmallRocketMultiMeshConfig(5, 5, 2, 1)
class SmallRocket6x6Core2x1MeshConfig extends AnySmallRocketMultiMeshConfig(6, 6, 2, 1)
class SmallRocket7x7Core2x1MeshConfig extends AnySmallRocketMultiMeshConfig(7, 7, 2, 1)
class SmallRocket8x8Core2x1MeshConfig extends AnySmallRocketMultiMeshConfig(8, 8, 2, 1)
class SmallRocket9x9Core2x1MeshConfig extends AnySmallRocketMultiMeshConfig(9, 9, 2, 1)
class SmallRocket10x10Core2x1MeshConfig extends AnySmallRocketMultiMeshConfig(10, 10, 2, 1)



class SmallRocket2x2Core2x2MeshConfig extends AnySmallRocketMultiMeshConfig(2, 2, 2, 2)
class SmallRocket3x3Core2x2MeshConfig extends AnySmallRocketMultiMeshConfig(3, 3, 2, 2)
class SmallRocket4x4Core2x2MeshConfig extends AnySmallRocketMultiMeshConfig(4, 4, 2, 2)
class SmallRocket5x5Core2x2MeshConfig extends AnySmallRocketMultiMeshConfig(5, 5, 2, 2)
class SmallRocket6x6Core2x2MeshConfig extends AnySmallRocketMultiMeshConfig(6, 6, 2, 2)
class SmallRocket7x7Core2x2MeshConfig extends AnySmallRocketMultiMeshConfig(7, 7, 2, 2)
class SmallRocket8x8Core2x2MeshConfig extends AnySmallRocketMultiMeshConfig(8, 8, 2, 2)
class SmallRocket9x9Core2x2MeshConfig extends AnySmallRocketMultiMeshConfig(9, 9, 2, 2)
class SmallRocket10x10Core2x2MeshConfig extends AnySmallRocketMultiMeshConfig(10, 10, 2, 2)


