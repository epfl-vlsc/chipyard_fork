package chipyard.example.simulation

import org.chipsalliance.cde.config.{Config}


abstract class AbstractSmallBoomConfig(n: Int) extends Config(
    new boom.common.WithNSmallBooms(n = n) ++
    new SimulationAbstractConfig
)

class SmallBoom1CoreConfig extends AbstractSmallBoomConfig(1)
class SmallBoom2CoreConfig extends AbstractSmallBoomConfig(2)
class SmallBoom4CoreConfig extends AbstractSmallBoomConfig(4)
class SmallBoom8CoreConfig extends AbstractSmallBoomConfig(8)
class SmallBoom16CoreConfig extends AbstractSmallBoomConfig(16)


abstract class AbstractLargeBoomConfig(n: Int) extends Config(
    new boom.common.WithNLargeBooms(n = n) ++
    new SimulationAbstractConfig
)

class LargeBoom1CoreConfig extends AbstractLargeBoomConfig(1)
class LargeBoom2CoreConfig extends AbstractLargeBoomConfig(2)
class LargeBoom4CoreConfig extends AbstractLargeBoomConfig(4)
class LargeBoom8CoreConfig extends AbstractLargeBoomConfig(8)


