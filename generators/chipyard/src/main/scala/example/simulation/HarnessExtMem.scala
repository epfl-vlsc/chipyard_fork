package example.simulation

import barstools.iocell.chisel._
import chipyard._
import chipyard.clocking.ClockWithFreq
import chipyard.clocking.HasChipyardPRCI
import chipyard.harness._
import chipyard.iobinders.GetSystemParameters
import chipyard.iobinders.JTAGChipIO
import chisel3._
import chisel3.experimental.Analog
import chisel3.experimental.BaseModule
import chisel3.experimental.DataMirror
import chisel3.experimental.Direction
import chisel3.experimental.IntParam
import chisel3.util._
import freechips.rocketchip.amba.AMBACorrupt
import freechips.rocketchip.amba.axi4.AXI4Buffer
import freechips.rocketchip.amba.axi4.AXI4Bundle
import freechips.rocketchip.amba.axi4.AXI4EdgeParameters
import freechips.rocketchip.amba.axi4.AXI4Fragmenter
import freechips.rocketchip.amba.axi4.AXI4MasterNode
import freechips.rocketchip.amba.axi4.AXI4Parameters
import freechips.rocketchip.amba.axi4.AXI4RAM
import freechips.rocketchip.amba.axi4.AXI4SlaveNode
import freechips.rocketchip.amba.axi4.AXI4SlaveParameters
import freechips.rocketchip.amba.axi4.AXI4SlavePortParameters
import freechips.rocketchip.amba.axi4.AXI4Xbar
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.diplomacy.DiplomaticSRAM
import freechips.rocketchip.diplomacy.InModuleBody
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.diplomacy.LazyModuleImpLike
import freechips.rocketchip.diplomacy.RegionType
import freechips.rocketchip.diplomacy.SimpleLazyModule
import freechips.rocketchip.diplomacy.TransferSizes
// import freechips.rocketchip.subsystem._
import freechips.rocketchip.system.SimAXIMem
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.Config
import org.chipsalliance.cde.config.Field
import org.chipsalliance.cde.config.Parameters
import sifive.blocks.devices.gpio._
import sifive.blocks.devices.spi._
import sifive.blocks.devices.uart._
import testchipip._
import tracegen.TraceGenSystemModuleImp
import freechips.rocketchip.diplomacy.Device
import freechips.rocketchip.diplomacy.DeviceRegName
import freechips.rocketchip.tilelink.TLManagerNode
import freechips.rocketchip.tilelink.TLSlavePortParameters
import freechips.rocketchip.tilelink.Atomics
import freechips.rocketchip.diplomacy.BundleBridgeSource
import freechips.rocketchip.tilelink.TLRAMErrors
import freechips.rocketchip.tilelink.TLSlaveParameters
import freechips.rocketchip.tilelink.TLMessages
import freechips.rocketchip.tilelink.TLEdgeParameters
import freechips.rocketchip.tilelink.TLClientNode
import freechips.rocketchip.tilelink.TLXbar
import freechips.rocketchip.tilelink.TLBuffer
import freechips.rocketchip.tilelink.TLBundle
import freechips.rocketchip.subsystem.CanHaveMasterAXI4MemPort
import freechips.rocketchip.subsystem.ExtMem
import freechips.rocketchip.subsystem.CanHaveMasterAXI4MMIOPort
import freechips.rocketchip.subsystem.ExtBus
import freechips.rocketchip.tilelink.TLAdapterNode
import freechips.rocketchip.tilelink.TLFragmenter
import freechips.rocketchip.regmapper.IORegisterRouter
import freechips.rocketchip.regmapper.RegisterRouterParams
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.tilelink.HasTLControlRegMap
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.subsystem


case class SimMemorySizeParam(size: BigInt)
case object ExtSimMemSize extends Field[Option[SimMemorySizeParam]]

class WithExtSimMemSize(size: BigInt) extends Config((site, here, up) => {
  case ExtSimMemSize => Some(SimMemorySizeParam(size))
})

class WithExtTLMemSize(n: BigInt) extends Config((site, here, up) => {
  case ExtTLMem => up(ExtTLMem, site).map(x => x.copy(master = x.master.copy(size = n)))
})
class WithSimAXIMemHexPlusArgs extends OverrideHarnessBinder({
  (system: CanHaveMasterAXI4MemPort, th: HasHarnessInstantiators, ports: Seq[ClockedAndResetIO[AXI4Bundle]]) => {
    val p: Parameters = chipyard.iobinders.GetSystemParameters(system)
    require(ports.length == 1, "expected exactly one Axi memory")
    (ports zip system.memAXI4Node.edges.in).map { case (port, edge) =>
      val mem = LazyModule(
                    new SimAXIMemGen(edge,
                        size = p(ExtSimMemSize).get.size, // do not use the ExtMem
                        base = p(ExtMem).get.master.base,
                        memoryGen =
                          (lanes, bits, size) => Module(new SimRAMLoadHex(lanes = lanes, bits = bits, size = size))
                    )(p)
                )
      Module(mem.module).suggestName("mem_axi")
      mem.io_axi4.head <> port.bits
    }
  }
})

class WithSimAXIMMIOToHostSnooper(toHostOffset: Int = 0) extends OverrideHarnessBinder({
  (system: CanHaveMasterAXI4MMIOPort, th: HasHarnessInstantiators, ports: Seq[ClockedAndResetIO[AXI4Bundle]]) => {
    val p: Parameters = chipyard.iobinders.GetSystemParameters(system)
    require(ports.length == 1, s"expected exactly one AXI MMIO port but have ${ports.length}")
    (ports zip system.mmioAXI4Node.edges.in).map { case (port, edge) =>
      val mmio = LazyModule(
        new SimAXIMemGen(
          edge,
          size = 1024,
          base = p(ExtBus).get.base,
          memoryGen = (lanes, bits, size) => Module(new SimMMIOToHostSnooper(lanes = lanes, bits = bits, size = size, toHostOffset = toHostOffset))
        )(p)
      )
      withClockAndReset(port.clock, port.reset) {
        Module(mmio.module).suggestName("mmio_mem_axi")
      }
      mmio.io_axi4.head <> port.bits
    }
  }
})


class WithSimTLMemHexPlusArgs extends OverrideHarnessBinder({
  (system: CanHaveMasterTLMemPort, th: HasHarnessInstantiators, ports: Seq[ClockedAndResetIO[TLBundle]]) => {
    val p: Parameters = chipyard.iobinders.GetSystemParameters(system)
    require(ports.length == 1, "expected exactly one TL memory")
    (ports zip system.memTLNode.edges.in).map { case (port, edge) =>
      val mem = LazyModule(
                    new SimTLMemGen(edge,
                        size = p(ExtSimMemSize).get.size, // do not use the ExtMem
                        base = p(ExtTLMem).get.master.base,
                        memoryGen =
                          (lanes, bits, size) => Module(new SimRAMLoadHex(lanes = lanes, bits = bits, size = size))
                    )(p)
                )
      Module(mem.module).suggestName("mem_tl")
      mem.io_tl.head <> port.bits
    }
  }
})

class WithSimTLMMIOToHostSnooper(toHostOffset: Int = 0) extends OverrideHarnessBinder({
  (system: CanHaveCustomMasterTLMMIOPort, th: HasHarnessInstantiators, ports: Seq[ClockedAndResetIO[TLBundle]]) => {
    val p: Parameters = chipyard.iobinders.GetSystemParameters(system)
    require(ports.length == 1, s"expected exactly one TL MMIO port but have ${ports.length}")
    (ports zip system.mmioTLNode.edges.in).map { case (port, edge) =>
      val mmio = LazyModule(
        new SimTLMemGen(
          edge,
          size = 1024,
          base = p(ExtTLBus).get.base,
          memoryGen = (lanes, bits, size) => Module(new SimMMIOToHostSnooper(lanes = lanes, bits = bits, size = size, toHostOffset = toHostOffset))
        )(p)
      )

      Module(mmio.module).suggestName("mmio_mem_tl")

      mmio.io_tl.head <> port.bits
    }
  }
})

class SimAXIMemGen(edge: AXI4EdgeParameters, size: BigInt, base: BigInt, memoryGen: SimRAMGenerator)(implicit p: Parameters)
    extends SimpleLazyModule {
    val node = AXI4MasterNode(List(edge.master))
    val addressSet = AddressSet.misaligned(base, size)
    require(addressSet.length == 1, f"0x${size}%x is too large a simulation memory!")
    val srams = addressSet.map { aSet =>
        LazyModule(new SimAXI4RAM(
            address = aSet,
            beatBytes = edge.bundle.dataBits / 8,
            wcorrupt =  edge.slave.requestKeys.contains(AMBACorrupt),
            memoryGen = memoryGen
        ))
    }
    val xbar = AXI4Xbar()
    srams.foreach{ s => s.node := AXI4Buffer() := AXI4Fragmenter() := xbar }
    xbar := node
    val io_axi4 = InModuleBody { node.makeIOs() }
}



class SimTLMemGen(edge: TLEdgeParameters, size: BigInt, base: BigInt, memoryGen: SimRAMGenerator)(implicit p: Parameters) extends SimpleLazyModule {
  val node = TLClientNode(List(edge.master))

  val srams = AddressSet.misaligned(base, size).map { aSet =>
    LazyModule(new SimTLRAM(
      address = aSet,
      beatBytes = edge.bundle.dataBits / 8,
      atomics = true,
      cacheable = true,
      executable = true,
      memoryGen = memoryGen))
  }

  val xbar = TLXbar()

  srams.foreach{ s =>
      s.node :=
        TLFragmenter(
          minSize = p(ExtTLMem).get.master.beatBytes,
          p(ExtTLMem).get.master.maxXferBytes
        ) :=
        TLBuffer() :=
        xbar
  }
  xbar := node
  val io_tl = InModuleBody { node.makeIOs() }
}

class SimRAMIO (lanes: Int, bits: Int, size: Int) extends Bundle {
    val addr = Input(UInt(log2Ceil(size).W))
    val rdata = Output(UInt((bits * lanes).W))
    val wdata = Input(UInt((bits * lanes).W))
    val wstrb = Input(UInt(lanes.W))
    val wen = Input(Bool())
    val ren = Input(Bool())
    val clock = Input(Clock())
    val reset = Input(Reset())
}
trait AbstractSimRAM {
    val io: SimRAMIO
}

trait SimRAMGenerator extends Function3[Int, Int, Int, AbstractSimRAM]{
    def apply(lanes: Int, bits: Int, size: Int): AbstractSimRAM
}

class SimAXI4RAM(
    address: AddressSet,
    cacheable: Boolean = true,
    executable: Boolean = true,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil,
    wcorrupt: Boolean = true,
    memoryGen: SimRAMGenerator)
  (implicit p: Parameters) extends DiplomaticSRAM(address, beatBytes, devName)
{

  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = List(address) ++ errors,
      regionType    = if (cacheable) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable    = executable,
      supportsRead  = TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes),
      interleavedId = Some(0))),
    beatBytes  = beatBytes,
    requestKeys = if (wcorrupt) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))

  private val outer = this

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, edgeIn) = node.in(0)
    val laneDataBits = 8


    val mem =
        memoryGen(
            lanes = beatBytes,
            size = (BigInt(1) << mask.filter(b => b).size).toInt,
            bits = laneDataBits)
    mem.io := DontCare
    mem.io.wen := false.B
    mem.io.ren := false.B
    mem.io.wstrb := 0.U

    mem.io.clock := clock
    mem.io.reset := reset.asBool

    val eccCode = None
    val address = outer.address

    val corrupt = if (edgeIn.bundle.requestFields.contains(AMBACorrupt)) Some(SyncReadMem(1 << mask.filter(b=>b).size, UInt(2.W))) else None

    val r_addr = Cat((mask zip (in.ar.bits.addr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    val w_addr = Cat((mask zip (in.aw.bits.addr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    val r_sel0 = address.contains(in.ar.bits.addr)
    val w_sel0 = address.contains(in.aw.bits.addr)

    val w_full = RegInit(false.B)
    val w_id   = Reg(UInt())
    val w_echo = Reg(BundleMap(in.params.echoFields))
    val r_sel1 = RegNext(r_sel0)
    val w_sel1 = RegNext(w_sel0)

    when (in. b.fire) { w_full := false.B }
    when (in.aw.fire) { w_full := true.B }

    when (in.aw.fire) {
      w_id := in.aw.bits.id
      w_sel1 := w_sel0
      w_echo :<= in.aw.bits.echo
    }


    when (in.aw.fire && w_sel0) {
      mem.io.wen := true.B
      mem.io.wstrb := in.w.bits.strb
      mem.io.wdata := in.w.bits.data
      mem.io.addr := w_addr
      corrupt.foreach { _.write(w_addr, in.w.bits.user(AMBACorrupt).asUInt) }
    }

    in. b.valid := w_full
    in.aw.ready := in. w.valid && (in.b.ready || !w_full)
    in. w.ready := in.aw.valid && (in.b.ready || !w_full)

    in.b.bits.id   := w_id
    in.b.bits.resp := Mux(w_sel1, AXI4Parameters.RESP_OKAY, AXI4Parameters.RESP_DECERR)
    in.b.bits.echo :<= w_echo

    val r_full = RegInit(false.B)
    val r_id   = Reg(UInt())
    val r_echo = Reg(BundleMap(in.params.echoFields))

    when (in. r.fire) { r_full := false.B }
    when (in.ar.fire) { r_full := true.B }

    when (in.ar.fire) {
      r_id := in.ar.bits.id
      r_sel1 := r_sel0
      r_echo :<= in.ar.bits.echo
    }

    val ren = in.ar.fire
    mem.io.ren := ren
    val rdata = mem.io.rdata holdUnless RegNext(ren)
    val rcorrupt = corrupt.map(_.readAndHold(r_addr, ren)(0)).getOrElse(false.B)
    when(ren) {
        mem.io.addr := r_addr
    }
    in. r.valid := r_full
    in.ar.ready := in.r.ready || !r_full

    in.r.bits.id   := r_id
    in.r.bits.resp := Mux(r_sel1, Mux(rcorrupt, AXI4Parameters.RESP_SLVERR, AXI4Parameters.RESP_OKAY), AXI4Parameters.RESP_DECERR)
    in.r.bits.data := rdata
    in.r.bits.echo :<= r_echo
    in.r.bits.last := true.B
  }
}

// class SimRAMLoadHex(val lanes: Int, val bits: Int, val size: Int) extends AbstractSimRAM {
//   val inner = Module(new SimRAMLoadHexBB(lanes = lanes, bits = bits, size = size))
//   inner.io.addr := io.addr
//   inner.io.wdata := io.wdata
//   inner.io.wen := io.wen
//   inner.io.clock := io.clock
//   inner.io.ren := io.ren
//   io.rdata := inner.io.rdata
//   inner.io.wstrb := io.wstrb
// }

class SimMMIOToHostSnooper(toHostOffset: Int, lanes: Int, bits: Int, size: Int) extends  BlackBox(
    Map(
      "OFFSET" -> IntParam(toHostOffset),
      "LANES" -> IntParam(lanes),
      "BITS" -> IntParam(bits),
      "SIZE" -> IntParam(size)
    )
  ) with HasBlackBoxInline with AbstractSimRAM {

    override val io = IO(new SimRAMIO(lanes = lanes, bits = bits, size = size))
    val moduleName = this.getClass.getSimpleName

    setInline(s"${moduleName}.sv",
        s"""|module $moduleName #(parameter SIZE = 8192,
            |                     parameter BITS = 8,
            |                     parameter LANES = 8,
            |                     parameter OFFSET = 'h0,
            |                     parameter ADDR_BITS = $$clog2(SIZE)
            |)
            |(
            |   input  wire [ADDR_BITS - 1 : 0] addr,
            |   input  wire [BITS * LANES - 1 : 0] wdata,
            |   input  wire [LANES - 1 : 0] wstrb,
            |   input  wire wen,
            |   input  wire ren,
            |   input  wire clock,
            |   input  wire reset,
            |   output wire [BITS * LANES - 1 : 0] rdata
            |);
            |
            |
            |   logic [ADDR_BITS - 1 : 0] addr_q;
            |   logic wen_q;
            |   logic [LANES - 1 : 0] wstrb_q;
            |   logic [BITS * LANES - 1 : 0] wdata_q;
            |   logic [63 : 0] cycle_counter = 0;
            |   assign rdata = 0;
            |   always_ff @(posedge clock) cycle_counter <= cycle_counter + 1;
            |   always_ff @(posedge clock) begin
            |     if (reset) begin
            |         wen_q <= 0;
            |         wstrb_q <= 0;
            |     end else if (wen) begin
            |         addr_q <= addr;
            |         wen_q <= wen;
            |         wstrb_q <= wstrb;
            |         wdata_q <= wdata;
            |       end
            |   end
            |`ifndef SYNTHESIS
            |   always_ff @(posedge clock) begin
            |          if (wen_q && wstrb_q[3:0] == 4'b1111 && addr_q == OFFSET) begin
            |            if (wdata_q[31:0] != 1) begin
            |              $$display("@%d: Test failed with toHost %d", cycle_counter, wdata_q);
            |              $$stop;
            |            end else begin
            |              $$display("@%d: Test passed", cycle_counter);
            |              $$finish;
            |            end
            |          end else if (wen_q && wstrb_q != 0) begin
            |              $$display("@%d: Unexpected MMIO write\\n\\taddr: 0x%h wdata: 0x%h wstrb: %b", cycle_counter, addr_q, wdata_q, wstrb_q);
            |              // $$stop;
            |          end
            |   end
            |`endif
            |endmodule
            |
            |""".stripMargin

    )
}

class SimRAMLoadHex(lanes: Int, bits: Int, size: Int) extends
    BlackBox(Map(
        "SIZE" -> IntParam(size),
        "BITS" -> IntParam(bits),
        "LANES" -> IntParam(lanes)))
         with HasBlackBoxInline
         with AbstractSimRAM
    {

    override val io = IO(new SimRAMIO(lanes = lanes, bits = bits, size = size))

    val moduleName = this.getClass.getSimpleName
    setInline(s"${moduleName}.sv",
        s"""|module $moduleName #(parameter SIZE=8192,
            |                     parameter BITS=8,
            |                     parameter LANES=8,
            |                     parameter ADDR_BITS = $$clog2(SIZE)
            |)
            |(
            |   input  wire [ADDR_BITS - 1 : 0] addr,
            |   input  wire [BITS * LANES - 1 : 0] wdata,
            |   input  wire [LANES - 1 : 0] wstrb,
            |   input  wire wen,
            |   input  wire ren,
            |   input  wire clock,
            |   input  wire reset,
            |   output wire [BITS * LANES - 1 : 0] rdata
            |);
            |
            |   logic [BITS * LANES - 1 : 0] storage [0 : SIZE - 1];
            |
            |   typedef logic [7:0][255:0] sstr_t;
            |   logic [63:0] cycle_counter = '0;
            |   always_ff @(posedge clock) cycle_counter <= cycle_counter + 1;
            |`ifndef SYNTHESIS
            |   initial begin: load_data
            |       sstr_t filename = '0;
            |       if (!$$value$$plusargs("binary=%s", filename)) begin
            |           $$display("+binary=HEXFILE is required.");
            |           $$stop;
            |       end
            |       $$readmemh(filename, storage);
            |   end
            |`endif
            |   logic [ADDR_BITS - 1 : 0] addr_q;
            |   always_ff @(posedge clock) begin
            |       if (wen) begin
            |           for (int i = 0; i < LANES; i = i + 1) begin
            |               if (wstrb[i]) begin
            |                   storage[addr][i * BITS +: BITS] <= wdata[i * BITS +: BITS];
            |               end
            |           end
            |       end
            |       if (ren) begin
            |           addr_q <= addr;
            |       end
            |   end
            |   assign rdata = storage[addr_q];
            |endmodule
            |
            |""".stripMargin
    )


}


class SimTLRAM(
    address: AddressSet,
    cacheable: Boolean = true,
    executable: Boolean = true,
    atomics: Boolean = false,
    beatBytes: Int = 4,
    ecc: ECCParams = ECCParams(),
    sramReg: Boolean = false, // drive SRAM data output directly into a register => 1 cycle longer response
    val devName: Option[String] = None,
    val dtsCompat: Option[Seq[String]] = None,
    val devOverride: Option[Device with DeviceRegName] = None,
    memoryGen: SimRAMGenerator
  )(implicit p: Parameters) extends DiplomaticSRAM(address, beatBytes, devName, dtsCompat, devOverride)
{
  val eccBytes = ecc.bytes
  val code = ecc.code
  require (eccBytes  >= 1 && isPow2(eccBytes))
  require (beatBytes >= 1 && isPow2(beatBytes))
  require (eccBytes <= beatBytes, s"TLRAM eccBytes (${eccBytes}) > beatBytes (${beatBytes}). Use a WidthWidget=>Fragmenter=>SRAM if you need high density and narrow ECC; it will do bursts efficiently")

  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(
    Seq(TLSlaveParameters.v1(
      address            = List(address),
      resources          = resources,
      regionType         = if (cacheable) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable         = executable,
      supportsGet        = TransferSizes(1, beatBytes),
      supportsPutPartial = TransferSizes(1, beatBytes),
      supportsPutFull    = TransferSizes(1, beatBytes),
      supportsArithmetic = if (atomics) TransferSizes(1, beatBytes) else TransferSizes.none,
      supportsLogical    = if (atomics) TransferSizes(1, beatBytes) else TransferSizes.none,
      fifoId             = Some(0))), // requests are handled in order
    beatBytes  = beatBytes,
    minLatency = 1))) // no bypass needed for this device

  val notifyNode = ecc.notifyErrors.option(BundleBridgeSource(() => new TLRAMErrors(ecc, log2Ceil(address.max)).cloneType))

  private val outer = this

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this){
    val (in, edge) = node.in(0)

    val indexBits = (outer.address.mask & ~(beatBytes-1)).bitCount
    val width = code.width(eccBytes*8)
    val lanes = beatBytes/eccBytes
    // val mem = makeSinglePortedByteWriteSeqMem(
    //   size = BigInt(1) << indexBits,
    //   lanes = lanes,
    //   bits = width)
    val mem =
      memoryGen(
          lanes = beatBytes,
          size = (BigInt(1) << indexBits).toInt,
          bits = width)
    mem.io := DontCare
    mem.io.wen := false.B
    mem.io.ren := false.B
    mem.io.wstrb := 0.U

    mem.io.clock := clock
    mem.io.reset := reset.asBool

    val eccCode = Some(ecc.code)
    val address = outer.address
    val laneDataBits = eccBytes * 8

    /* This block has a three-stage pipeline
     * Stage A is the combinational request from TileLink A channel
     * Stage R corresponds to an accepted request
     * Stage D registers the result of an SRAM read (if any)
     *
     * The TileLink D channel response comes from
     *   - stage D for corected reads or AMOs
     *   - stage R for everything else
     *   - However, to increase maximum operating frequency, the
     *     stage R responses can be configured to come from stage D
     *
     * For sub-ECC granule writes and atomic operations:
     *   - stage A sets up the read for the old data value
     *   - stage R is used to gather the result from SRAM to registers
     *   - stage D corrects ECC, applies the ALU, and sets up SRAM write
     *
     * For super-ECC granule writes:
     *   - stage A sets up the write
     *
     * For reads:
     *   - stage A sets up the read
     *   - stage R drives the uncorrected data with valid based on ECC validity
     *   - stage D sets up the correction, if any
     *
     * When stage D needs to perform a write (AMO, sub-ECC write, or ECC correction):
     *   - there is a WaW or WaR hazard vs. the operation in stage R
     *     - for sub-ECC writes and atomics, we ensure stage R has a bubble
     *     - for ECC correction, we cause stage R to be replayed (and reject stage A twice)
     *   - there is a structural hazard competing with stage A for SRAM access
     *     - stage D always wins (stage A is rejected)
     *   - on ECC correction, there is a structural hazard competing with stage R for the response channel
     *     - stage D always wins (stage R is replayed)
     */
    println(s"edge params: ${edge.bundle}")
    // D stage registers from R
    val d_full      = RegInit(false.B)
    val d_respond   = Reg(Bool())
    val d_opcode    = Reg(UInt(3.W))
    val d_param     = Reg(UInt(3.W))
    val d_size      = Reg(UInt((edge.bundle.sizeBits).W))
    val d_source    = Reg(UInt(edge.bundle.sourceBits.W))
    val d_read      = Reg(Bool())
    val d_atomic    = Reg(Bool())
    val d_sublane   = Reg(Bool())
    val d_address   = Reg(UInt(edge.bundle.addressBits.W))
    val d_mask      = Reg(UInt(beatBytes.W))
    val d_rmw_data  = Reg(UInt((8*beatBytes).W))
    val d_poison    = Reg(Bool())
    val d_raw_data  = Reg(Vec(lanes, Bits(width.W)))

    // R stage registers from A
    val r_full      = RegInit(false.B)
    val r_opcode    = Reg(UInt(3.W))
    val r_param     = Reg(UInt(3.W))
    val r_size      = Reg(UInt((edge.bundle.sizeBits).W))
    val r_source    = Reg(UInt(edge.bundle.sourceBits.W))
    val r_read      = Reg(Bool())
    val r_atomic    = Reg(Bool())
    val r_sublane   = Reg(Bool())
    val r_address   = Reg(UInt(edge.bundle.addressBits.W))
    val r_mask      = Reg(UInt(beatBytes.W))
    val r_rmw_data  = Reg(UInt((8*beatBytes).W))
    val r_poison    = Reg(Bool())
    val r_raw_data  = Wire(Vec(lanes, Bits(width.W)))

    // Decode raw SRAM output
    val d_decoded       = d_raw_data.map(lane => code.decode(lane))
    val d_corrected     = Cat(d_decoded.map(_.corrected).reverse)
    val d_uncorrected   = Cat(d_decoded.map(_.uncorrected).reverse)
    val d_correctable   = d_decoded.map(_.correctable)
    val d_uncorrectable = d_decoded.map(_.uncorrectable)
    val d_need_fix      = d_correctable.reduce(_ || _)
    val d_lanes         = Cat(Seq.tabulate(lanes) { i => d_mask(eccBytes*(i+1)-1, eccBytes*i).orR }.reverse)
    val d_lane_error    = Cat(d_uncorrectable.reverse) & d_lanes
    val d_error         = d_lane_error.orR

    val r_decoded       = r_raw_data.map(lane => code.decode(lane))
    val r_corrected     = Cat(r_decoded.map(_.corrected).reverse)
    val r_uncorrected   = Cat(r_decoded.map(_.uncorrected).reverse)
    val r_correctable   = r_decoded.map(_.correctable)
    val r_uncorrectable = r_decoded.map(_.uncorrectable)
    val r_need_fix      = r_correctable.reduce(_ || _)
    val r_lanes         = Cat(Seq.tabulate(lanes) { i => r_mask(eccBytes*(i+1)-1, eccBytes*i).orR }.reverse)
    val r_lane_error    = Cat(r_uncorrectable.reverse) & r_lanes
    val r_error         = r_lane_error.orR

    // Out-of-band notification of any faults
    notifyNode.foreach { nnode =>
      nnode.bundle.correctable.foreach { c =>
        c.valid := d_need_fix && d_full && (d_atomic || d_read || d_sublane)
        c.bits  := d_address
      }
      nnode.bundle.uncorrectable.foreach { u =>
        u.valid := d_error && d_full && (d_atomic || d_read || d_sublane)
        u.bits  := d_address
      }
    }

    // What does D-stage want to write-back?
    // Make an ALU if we need one
    val d_updated = if (atomics) {
      val alu = Module(new Atomics(edge.bundle))
      alu.io.write     := false.B
      alu.io.a.opcode  := d_opcode
      alu.io.a.param   := d_param
      alu.io.a.size    := d_size
      alu.io.a.source  := 0.U
      alu.io.a.address := 0.U
      alu.io.a.data    := d_rmw_data
      alu.io.a.mask    := d_mask
      alu.io.a.corrupt := false.B
      alu.io.data_in   := d_corrected
      alu.io.data_out
    } else {
      Cat(Seq.tabulate(beatBytes) { i =>
        val upd = d_mask(i) && !d_read
        val rmw = d_rmw_data (8*(i+1)-1, 8*i)
        val fix = d_corrected(8*(i+1)-1, 8*i) // safe to use, because D-stage write-back always wins arbitration
        Mux(upd, rmw, fix)
      }.reverse)
    }

    // Stage D always wins control of the response channel
    val d_win = d_full && d_respond
    val d_mux = if (sramReg) true.B else d_win
    val out_aad = Mux(d_mux, d_read || d_atomic, r_read || r_atomic)
    in.d.bits.opcode  := Mux(out_aad, TLMessages.AccessAckData, TLMessages.AccessAck)
    in.d.bits.param   := 0.U
    in.d.bits.size    := Mux(d_mux, d_size,   r_size)
    in.d.bits.source  := Mux(d_mux, d_source, r_source)
    in.d.bits.sink    := 0.U
    in.d.bits.denied  := false.B
    in.d.bits.data    := Mux(d_mux, d_corrected, r_uncorrected)
    in.d.bits.corrupt := Mux(d_mux, d_error, r_error) && out_aad

    val mem_active_valid = Seq(property.CoverBoolean(in.d.valid, Seq("mem_active")))
    val data_error = Seq(
      property.CoverBoolean(!d_need_fix && !d_error , Seq("no_data_error")),
      property.CoverBoolean(d_need_fix && !in.d.bits.corrupt, Seq("data_correctable_error_not_reported")),
      property.CoverBoolean(d_error && in.d.bits.corrupt, Seq("data_uncorrectable_error_reported")))

    val error_cross_covers = new property.CrossProperty(Seq(mem_active_valid, data_error), Seq(), "Ecc Covers")
    property.cover(error_cross_covers)

    // Does the D stage want to perform a write?
    // It's important this reduce to false.B when eccBytes=1 && atomics=false && canCorrect=false
    val d_wb = d_full && (d_sublane || d_atomic || (d_read && d_need_fix))
    // Formulate an R response unless there is a data output fix to perform
    // It's important this reduce to false.B for sramReg and true.B for !code.canCorrect
    val r_respond = !sramReg.B && (!r_need_fix || !(r_read || r_atomic))
    // Resolve WaW and WaR hazard when D performs an update (only happens on ECC correction)
    // It's important this reduce to false.B unless code.canDetect
    val r_replay = RegNext(r_full && d_full && d_read && d_need_fix)
    // r_full && d_wb => read ecc fault (we insert a buble for atomic/sublane)
    assert (!(r_full && d_wb) || (d_full && d_read && d_need_fix))

    // Pipeline control
    in.d.valid := (d_full && d_respond) || (r_full && r_respond && !d_wb && !r_replay)
    val d_ready = !d_respond || in.d.ready
    val r_ready = !d_wb && !r_replay && (!d_full || d_ready) && (!r_respond || (!d_win && in.d.ready))
    in.a.ready := !(d_full && d_wb) && (!r_full || r_ready) && (!r_full || !(r_atomic || r_sublane))

    val a_sublane = if (eccBytes == 1) false.B else
      in.a.bits.opcode === TLMessages.PutPartialData ||
      in.a.bits.size < log2Ceil(eccBytes).U
    val a_atomic = if (!atomics) false.B else
      in.a.bits.opcode === TLMessages.ArithmeticData ||
      in.a.bits.opcode === TLMessages.LogicalData
    val a_read = in.a.bits.opcode === TLMessages.Get

    // Forward pipeline stage from R to D
    when (d_ready) { d_full := false.B }
    when (r_full && r_ready) {
      d_full     := true.B
      d_respond  := !r_respond
      d_opcode   := r_opcode
      d_param    := r_param
      d_size     := r_size
      d_source   := r_source
      d_read     := r_read
      d_atomic   := r_atomic
      d_sublane  := r_sublane
      d_address  := r_address
      d_mask     := r_mask
      d_rmw_data := r_rmw_data
      d_poison   := r_poison
      d_raw_data := r_raw_data
    }

    // Forward pipeline stage from A to R
    when (r_ready) { r_full := false.B }
    when (in.a.fire()) {
      r_full     := true.B
      r_sublane  := a_sublane
      r_opcode   := in.a.bits.opcode
      r_param    := in.a.bits.param
      r_size     := in.a.bits.size
      r_source   := in.a.bits.source
      r_read     := a_read
      r_atomic   := a_atomic
      r_sublane  := a_sublane
      r_address  := in.a.bits.address
      r_poison   := in.a.bits.corrupt
      r_mask     := in.a.bits.mask
      when (!a_read) { r_rmw_data := in.a.bits.data }
    }

    // Split data into eccBytes-sized chunks:
    val a_data = VecInit(Seq.tabulate(lanes) { i => in.a.bits.data(eccBytes*8*(i+1)-1, eccBytes*8*i) })
    val r_data = VecInit(Seq.tabulate(lanes) { i => r_rmw_data(eccBytes*8*(i+1)-1, eccBytes*8*i) })
    val d_data = VecInit(Seq.tabulate(lanes) { i => d_updated(8*eccBytes*(i+1)-1, 8*eccBytes*i) })

    // Which data chunks get poisoned
    val a_poisonv = VecInit(Seq.fill(lanes) { in.a.bits.corrupt })
    val r_poisonv = VecInit(Seq.fill(lanes) { r_poison })
    val d_poisonv = VecInit(Seq.tabulate(lanes) { i =>
      val upd = d_mask(eccBytes*(i+1)-1, eccBytes*i)
      (!upd.andR && d_uncorrectable(i)) || d_poison // sub-lane writes should not correct uncorrectable
    })

    val a_lanes = Cat(Seq.tabulate(lanes) { i => in.a.bits.mask(eccBytes*(i+1)-1, eccBytes*i).orR }.reverse)

    // SRAM arbitration
    val a_fire = in.a.fire()
    val a_ren = a_read || a_atomic || a_sublane
    val r_ren = r_read || r_atomic || r_sublane
    val wen = d_wb || Mux(r_replay, !r_ren, a_fire && !a_ren)
    val ren = !wen && (a_fire || r_replay) // help Chisel infer a RW-port

    val addr   = Mux(d_wb, d_address, Mux(r_replay, r_address, in.a.bits.address))
    val sel    = Mux(d_wb, d_lanes,   Mux(r_replay, r_lanes,   a_lanes))
    val dat    = Mux(d_wb, d_data,    Mux(r_replay, r_data,    a_data))
    val poison = Mux(d_wb, d_poisonv, Mux(r_replay, r_poisonv, a_poisonv))
    val coded  = VecInit((dat zip poison) map { case (d, p) =>
      if (code.canDetect) code.encode(d, p) else code.encode(d)
    })

    val index = Cat(mask.zip((addr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    // r_raw_data := mem.read(index, ren) holdUnless RegNext(ren)
    mem.io.addr := index
    mem.io.ren := ren

    val rdata_vec = VecInit(Seq.tabulate(lanes) { i => mem.io.rdata(beatBytes * (i + 1) - 1, beatBytes * i)})

    r_raw_data := rdata_vec holdUnless RegNext(ren)

    when (wen) {
      mem.io.wdata := coded.asUInt
      mem.io.wen := true.B
      mem.io.addr := index
      mem.io.wstrb := sel
      // mem.write(index, coded, sel.asBools)
    }

    // Tie off unused channels
    in.b.valid := false.B
    in.c.ready := true.B
    in.e.ready := true.B
  }
}

case class TLMMIOToHostParams(base: BigInt = 0x400000)
case object TLMMIOToHostKey extends Field[Option[TLMMIOToHostParams]](None)

class TLMMIOToHostIO extends Bundle {
  val clock = Input(Clock())
  val input_valid = Input(Bool())
  val input_ready = Output(Bool())
  val data = Input(UInt(32.W))
}

trait HasTLMMIOToHostIO extends BaseModule {
  val io = IO(new TLMMIOToHostIO)
}

class TLMMIOToHostBlackBox extends BlackBox() with HasBlackBoxInline with HasTLMMIOToHostIO {
  val moduleName = this.getClass.getSimpleName
  setInline(
    moduleName,
    s"""|
        |module ${moduleName}(
        |    input wire clock,
        |    input wire input_valid,
        |    input wire [31:0] data,
        |    output wire input_ready
        |);
        |
        |    logic[63:0] counter = 0;
        |    always@(posedge clock) begin
        |        counter <= counter + 1;
        |        if (input_valid) begin
        |            if (data == 1) begin
        |                $$display("@%d: test passed!", counter);
        |                $$finish;
        |            end else begin
        |                $$display("@%d: test failed with toHost = %d!", counter, data);
        |                $$stop;
        |            end
        |        end
        |    end
        |endmodule
        |
        |""".stripMargin
  )
}

abstract class TLMMIOToHostModule(val params: TLMMIOToHostParams)(implicit p: Parameters)
  extends IORegisterRouter(
    RegisterRouterParams(
      name = "tl_mmio_tohost",
      compat = Seq("epfl-vlsc,tl_mmio_tohost"),
      base = params.base,
      beatBytes = 4
    ),
    new TLMMIOToHostIO
  ){


  lazy val module  = new Impl
  class Impl extends LazyModuleImp(this) {

    val bb = Module(new TLMMIOToHostBlackBox())

    val data = new DecoupledIO(UInt(32.W))

    bb.io.clock := clock
    bb.io.data := data.bits
    bb.io.input_valid := data.valid
    data.ready := bb.io.input_ready

    regmap(
      0x00 -> Seq(RegField.w(32, data))
    )
  }
}


class TLMMMIOToHostDevice(params: TLMMIOToHostParams)(implicit p: Parameters)
  extends TLMMIOToHostModule(params) with HasTLControlRegMap


trait CanHavePeripheryTLMMIOToHost { this: BaseSubsystem =>

  private val portName = "tl_mmio_tohost"

  val tl_mmio_tohost = p(TLMMIOToHostKey) match {
    case Some(params) =>
      val tohost = LazyModule(new TLMMMIOToHostDevice(params))
      pbus.coupleTo(portName) {
        tohost.node := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _
      }
      Some(tohost)
    case None => None
  }
}

class WithTLMMIOToHostDevice(base: BigInt) extends Config( (site, here, up) => {
  case TLMMIOToHostKey => Some(TLMMIOToHostParams(base))
})