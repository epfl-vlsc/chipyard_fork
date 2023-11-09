package chipyard.example.simulation

import org.chipsalliance.cde.config.{Config}


class AnySmallBoomBusConfig(nCores: Int) extends Config(
    new boom.common.WithNSmallBooms(n = nCores) ++
    new MinimalSimulationConfig
)

class SmallBoom1CoreBusConfig extends AnySmallBoomBusConfig(1)
class SmallBoom2CoreBusConfig extends AnySmallBoomBusConfig(2)
class SmallBoom3CoreBusConfig extends AnySmallBoomBusConfig(3)
class SmallBoom4CoreBusConfig extends AnySmallBoomBusConfig(4)
class SmallBoom6CoreBusConfig extends AnySmallBoomBusConfig(6)
class SmallBoom8CoreBusConfig extends AnySmallBoomBusConfig(8)

class AnyMediumBoomBusConfig(nCores: Int) extends Config(
    new boom.common.WithNMediumBooms(n = nCores) ++
    new MinimalSimulationConfig
)


class MediumBoom1CoreBusConfig extends AnyMediumBoomBusConfig(1)
class MediumBoom2CoreBusConfig extends AnyMediumBoomBusConfig(2)
class MediumBoom3CoreBusConfig extends AnyMediumBoomBusConfig(3)
class MediumBoom4CoreBusConfig extends AnyMediumBoomBusConfig(4)
class MediumBoom6CoreBusConfig extends AnyMediumBoomBusConfig(6)
class MediumBoom8CoreBusConfig extends AnyMediumBoomBusConfig(8)



class AnyLargeBoomBusConfig(nCores: Int) extends Config(
    new boom.common.WithNLargeBooms(n = nCores) ++
    new MinimalSimulationConfig
)


class LargeBoom1CoreBusConfig extends AnyLargeBoomBusConfig(1)
class LargeBoom2CoreBusConfig extends AnyLargeBoomBusConfig(2)
class LargeBoom3CoreBusConfig extends AnyLargeBoomBusConfig(3)
class LargeBoom4CoreBusConfig extends AnyLargeBoomBusConfig(4)
class LargeBoom6CoreBusConfig extends AnyLargeBoomBusConfig(6)
class LargeBoom8CoreBusConfig extends AnyLargeBoomBusConfig(8)




class AnyMegaBoomBusConfig(nCores: Int) extends Config(
    new boom.common.WithNMegaBooms(n = nCores) ++
    new MinimalSimulationConfig
)


class MegaBoom1CoreBusConfig extends AnyMegaBoomBusConfig(1)
class MegaBoom2CoreBusConfig extends AnyMegaBoomBusConfig(2)
class MegaBoom3CoreBusConfig extends AnyMegaBoomBusConfig(3)
class MegaBoom4CoreBusConfig extends AnyMegaBoomBusConfig(4)
class MegaBoom6CoreBusConfig extends AnyMegaBoomBusConfig(6)
class MegaBoom8CoreBusConfig extends AnyMegaBoomBusConfig(8)


