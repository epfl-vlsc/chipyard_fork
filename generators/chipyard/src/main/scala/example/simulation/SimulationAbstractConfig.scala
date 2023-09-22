package chipyard.example.simulation

import chisel3._

import org.chipsalliance.cde.config.{Config, Parameters, Field}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImpLike}
import freechips.rocketchip.amba.axi4.{AXI4Bundle, AXI4SlaveNode, AXI4MasterNode, AXI4EdgeParameters}
import freechips.rocketchip.subsystem.CanHaveMasterAXI4MemPort
import chipyard.harness.HasHarnessInstantiators
import testchipip.ClockedAndResetIO
import chipyard.harness.OverrideHarnessBinder
import freechips.rocketchip.system.{SimAXIMem}
import example.simulation.WithSimAXIMemHexPlusArgs
import example.simulation.WithSimAXIMMIOToHostSnooper
import example.simulation.WithSimTLMemHexPlusArgs
import example.simulation.WithSimTLMMIOToHostSnooper
import chipyard.config.WithTLBackingMemory
import chipyard.config.WithTLBackingMMIO
import chipyard.clocking.ChipyardPRCIControlKey
import freechips.rocketchip.devices.tilelink.CLINTKey

// Option to look up the bootrom from a different directory
class WithBootROMPrefixPath(prefixPath: String) extends Config((site, here, up) => {
  case freechips.rocketchip.devices.tilelink.BootROMLocated(x) =>
    up(freechips.rocketchip.devices.tilelink.BootROMLocated(x), site)
      .map(_.copy(contentFileName =
        s"${prefixPath}/bootrom.rv${site(freechips.rocketchip.tile.XLen)}.img"))
})

class WithDisableTileClockGating extends Config((site, here, up) => {
  case ChipyardPRCIControlKey =>
    up(ChipyardPRCIControlKey, site).copy(enableTileClockGating = false)
})
class WithDisableTileResetSetter extends Config((site, here, up) => {
  case ChipyardPRCIControlKey =>
    up(ChipyardPRCIControlKey, site).copy(enableTileResetSetting = false)
})

class WithNoCLINT extends Config((site, here, up) => {
  case CLINTKey => None
})

class SimulationAbstractConfig(freqMHz: Double = 1000.0) extends Config(
  new WithBootROMPrefixPath("sims/verilator/bootrom") ++
  new WithSimAXIMemHexPlusArgs ++
  new WithSimAXIMMIOToHostSnooper ++
  new freechips.rocketchip.subsystem.WithExtMemSize(0x20000) ++ // 128KiB
  // new chipyard.harness.WithBlackBoxSimMem(additionalLatency = 2) ++
  // new chipyard.harness.WithSimAXIMem ++
  new chipyard.config.WithNoDebug ++
  new chipyard.config.WithNoUART ++
  new chipyard.config.WithNoTraceIO ++
  new testchipip.WithNoSerialTL ++
  new freechips.rocketchip.subsystem.WithoutTLMonitors ++     // disable TL monitors to avoid having a large block of prints
  // new chipyard.harness.WithSimAXIMMIO ++                           // add SimAXIMem for axi4 mmio port, if enabled
  // new chipyard.harness.WithTieOffInterrupts ++                     // tie-off interrupt ports, if present
  // new chipyard.harness.WithTieOffL2FBusAXI ++                      // tie-off external AXI4 master, if present
  // new chipyard.harness.WithCustomBootPinPlusArg ++                 // drive custom-boot pin with a plusarg, if custom-boot-pin is present
  new chipyard.harness.WithClockAndResetFromHarness ++             // all Clock/Reset I/O in ChipTop should be driven by harnessClockInstantiator
  new chipyard.harness.WithHarnessBinderClockFreqMHz(freqMHz) ++ // default is 100, make it 1000.0 to match the outer clock
  new chipyard.config.WithPeripheryBusFrequency(freqMHz) ++           // 1000 MHz pbus
  new chipyard.config.WithMemoryBusFrequency(freqMHz) ++              // 1000 MHz mbus
  new chipyard.harness.WithAllClocksFromHarnessClockInstantiator ++ // get all clocks from the harness
  // The IOBinders instantiate ChipTop IOs to match desired digital IOs
  // IOCells are generated for "Chip-like" IOs, while simulation-only IOs are directly punched through
  new chipyard.iobinders.WithAXI4MemPunchthrough ++
  new chipyard.iobinders.WithAXI4MMIOPunchthrough ++
  new chipyard.iobinders.WithTLMemPunchthrough ++
  new chipyard.iobinders.WithL2FBusAXI4Punchthrough ++
  new chipyard.iobinders.WithBlockDeviceIOPunchthrough ++
  new chipyard.iobinders.WithNICIOPunchthrough ++
  new chipyard.iobinders.WithSerialTLIOCells ++
  new chipyard.iobinders.WithDebugIOCells ++
  new chipyard.iobinders.WithUARTIOCells ++
  new chipyard.iobinders.WithGPIOCells ++
  new chipyard.iobinders.WithSPIIOCells ++
  new chipyard.iobinders.WithTraceIOPunchthrough ++
  new chipyard.iobinders.WithExtInterruptIOCells ++


  // By default, punch out IOs to the Harness
  new chipyard.clocking.WithPassthroughClockGenerator ++
  new chipyard.clocking.WithClockGroupsCombinedByName(("uncore", Seq("sbus", "mbus", "pbus", "fbus", "cbus", "implicit"), Seq("tile"))) ++
  // new chipyard.config.WithPeripheryBusFrequency(500.0) ++           // Default 500 MHz pbus
  // new chipyard.config.WithMemoryBusFrequency(500.0) ++              // Default 500 MHz mbus

  // new testchipip.WithCustomBootPin ++                               // add a custom-boot-pin to support pin-driven boot address
  // new testchipip.WithBootAddrReg ++                                 // add a boot-addr-reg for configurable boot address
  new testchipip.WithSerialTLClientIdBits(4) ++                     // support up to 1 << 4 simultaneous requests from serialTL port
  new testchipip.WithSerialTLWidth(32) ++                           // fatten the serialTL interface to improve testing performance
  new testchipip.WithDefaultSerialTL ++                             // use serialized tilelink port to external serialadapter/harnessRAM
  new chipyard.config.WithDebugModuleAbstractDataWords(8) ++        // increase debug module data capacity
  new chipyard.config.WithBootROM ++                                // use default bootrom
  new chipyard.config.WithUART ++                                   // add a UART
  new chipyard.config.WithL2TLBs(1024) ++                           // use L2 TLBs
  new chipyard.config.WithNoSubsystemDrivenClocks ++                // drive the subsystem diplomatic clocks from ChipTop instead of using implicit clocks
  new chipyard.config.WithInheritBusFrequencyAssignments ++         // Unspecified clocks within a bus will receive the bus frequency if set
  new freechips.rocketchip.subsystem.WithNMemoryChannels(1) ++      // Default 1 memory channels
  // new freechips.rocketchip.subsystem.WithClockGateModel ++          // add default EICG_wrapper clock gate model
  new freechips.rocketchip.subsystem.WithJtagDTM ++                 // set the debug module to expose a JTAG port
  // new freechips.rocketchip.subsystem.WithNoMMIOPort ++              // no top-level MMIO master port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithNoSlavePort ++             // no top-level MMIO slave port (overrides default set in rocketchip)
  // new freechips.rocketchip.subsystem.WithInclusiveCache ++          // use Sifive L2 cache
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++    // one external interrupts
  new freechips.rocketchip.subsystem.WithDontDriveBusClocksFromSBus ++ // leave the bus clocks undriven by sbus
  new freechips.rocketchip.subsystem.WithCoherentBusTopology ++     // hierarchical buses including sbus/mbus/pbus/fbus/cbus/l2
  new freechips.rocketchip.subsystem.WithDTS("epfl-vlsc,chipyard", Nil) ++ // custom device name for DTS
  new freechips.rocketchip.system.BaseConfig
)

class SimpleSingleRocket extends Config(
  new freechips.rocketchip.subsystem.WithNSmallCores(n = 1) ++
  new SimulationAbstractConfig
)


class MinimalSimulationConfig(freqMHz: Double = 1000.0, extMemSize: Int = 0x20000 /*128 KiB*/) extends Config(
  // The HarnessBinders control generation of hardware in the TestHarness
  new freechips.rocketchip.subsystem.WithSynchronousRocketTiles ++
  new WithBootROMPrefixPath("sims/verilator/bootrom") ++
  new WithSimTLMemHexPlusArgs ++
  new WithSimTLMMIOToHostSnooper ++
  new WithTLBackingMemory ++
  new WithTLBackingMMIO ++
  new freechips.rocketchip.subsystem.WithExtMemSize(extMemSize) ++
  new WithDisableTileClockGating ++
  new WithDisableTileResetSetter ++
  // disable unnecessary stuff
  new WithNoCLINT ++
  new chipyard.config.WithNoDebug ++
  new chipyard.config.WithNoPLIC ++
  new chipyard.config.WithNoTraceIO ++
  new chipyard.config.WithNoUART ++
  new testchipip.WithNoSerialTL ++
  new testchipip.WithNoCustomBootPin ++
  new freechips.rocketchip.subsystem.WithoutTLMonitors ++
  new chipyard.harness.WithTieOffInterrupts ++                     // tie-off interrupt ports, if present
  new chipyard.harness.WithTieOffL2FBusAXI ++                      // tie-off external AXI4 master, if present
  new chipyard.harness.WithCustomBootPinPlusArg ++                 // drive custom-boot pin with a plusarg, if custom-boot-pin is present
  new chipyard.harness.WithClockAndResetFromHarness ++             // all Clock/Reset I/O in ChipTop should be driven by harnessClockInstantiator
  new chipyard.harness.WithAllClocksFromHarnessClockInstantiator ++
  // The IOBinders instantiate ChipTop IOs to match desired digital IOs
  // IOCells are generated for "Chip-like" IOs, while simulation-only IOs are directly punched through
  new chipyard.iobinders.WithAXI4MemPunchthrough ++
  new chipyard.iobinders.WithAXI4MMIOPunchthrough ++
  new chipyard.iobinders.WithTLMemPunchthrough ++
  new chipyard.iobinders.WithTLMMIOPunchthrough ++
  new chipyard.iobinders.WithL2FBusAXI4Punchthrough ++
  new chipyard.iobinders.WithSerialTLIOCells ++
  new chipyard.iobinders.WithDebugIOCells ++
  new chipyard.iobinders.WithUARTIOCells ++
  new chipyard.iobinders.WithGPIOCells ++
  new chipyard.iobinders.WithSPIIOCells ++
  new chipyard.iobinders.WithTraceIOPunchthrough ++
  new chipyard.iobinders.WithExtInterruptIOCells ++
  new chipyard.iobinders.WithCustomBootPin ++

  // By default, punch out IOs to the Harness
  new chipyard.clocking.WithPassthroughClockGenerator ++
  new chipyard.clocking.WithClockGroupsCombinedByName(("uncore", Seq("sbus", "mbus", "pbus", "fbus", "cbus", "implicit"), Seq("tile"))) ++
  new chipyard.harness.WithHarnessBinderClockFreqMHz(freqMHz) ++
  new chipyard.config.WithPeripheryBusFrequency(freqMHz) ++           // Default 500 MHz pbus
  new chipyard.config.WithMemoryBusFrequency(freqMHz) ++              // Default 500 MHz mbus

  new testchipip.WithCustomBootPin ++                               // add a custom-boot-pin to support pin-driven boot address
  new testchipip.WithBootAddrReg ++                                 // add a boot-addr-reg for configurable boot address
  new chipyard.config.WithL2TLBs(64) ++                             // use L2 TLBs
  new chipyard.config.WithNoSubsystemDrivenClocks ++                // drive the subsystem diplomatic clocks from ChipTop instead of using implicit clocks
  new chipyard.config.WithInheritBusFrequencyAssignments ++         // Unspecified clocks within a bus will receive the bus frequency if set
  new freechips.rocketchip.subsystem.WithNMemoryChannels(1) ++      // Default 1 memory channels
  new freechips.rocketchip.subsystem.WithNoSlavePort ++             // no top-level MMIO slave port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithInclusiveCache(capacityKB = 64) ++          // use Sifive L2 cache
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++    // no external interrupts
  new freechips.rocketchip.subsystem.WithDontDriveBusClocksFromSBus ++ // leave the bus clocks undriven by sbus
  new freechips.rocketchip.subsystem.WithCoherentBusTopology ++     // hierarchical buses including sbus/mbus/pbus/fbus/cbus/l2
  new freechips.rocketchip.subsystem.WithDTS("epfl-vlsc,chipyard", Nil) ++ // custom device name for DTS
  new freechips.rocketchip.system.BaseConfig)                       // "base" rocketchip system
