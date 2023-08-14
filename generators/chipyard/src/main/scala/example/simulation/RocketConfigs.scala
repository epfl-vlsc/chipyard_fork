package chipyard.example.simulation

import chipyard.example.simulation.SimulationAbstractConfig
import org.chipsalliance.cde.config.Config


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



