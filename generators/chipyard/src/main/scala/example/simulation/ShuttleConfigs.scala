package chipyard.example.simulation

import org.chipsalliance.cde.config.{Config}
import chipyard.example.simulation.SimulationAbstractConfig
import chipyard.example.simulation.MinimalSimulationConfig



abstract class AnyShuttleConfig(n: Int) extends Config(
    new shuttle.common.WithNShuttleCores(n = n) ++
    new MinimalSimulationConfig
)


class Shuttle1CoreConfig extends AnyShuttleConfig(1)
class Shuttle2CoreConfig extends AnyShuttleConfig(2)
class Shuttle4CoreConfig extends AnyShuttleConfig(4)
class Shuttle8CoreConfig extends AnyShuttleConfig(8)
class Shuttle16CoreConfig extends AnyShuttleConfig(16)
class Shuttle32CoreConfig extends AnyShuttleConfig(32)






