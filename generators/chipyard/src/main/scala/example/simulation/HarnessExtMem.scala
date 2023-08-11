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
import freechips.rocketchip.subsystem._
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


class WithSimAXIMemHexPlusArgs extends OverrideHarnessBinder({
  (system: CanHaveMasterAXI4MemPort, th: HasHarnessInstantiators, ports: Seq[ClockedAndResetIO[AXI4Bundle]]) => {
    val p: Parameters = chipyard.iobinders.GetSystemParameters(system)
    require(ports.length == 1, "expected exactly one Axi memory")
    (ports zip system.memAXI4Node.edges.in).map { case (port, edge) =>
      val mem = LazyModule(
                    new SimAXIMemGen(edge,
                        size = p(ExtMem).get.master.size,
                        base = p(ExtMem).get.master.base,
                        memoryGen =
                          (lanes, bits, size) => Module(new SimRAMLoadHex(lanes = lanes, bits = bits, size = size))
                    )(p)
                )
      Module(mem.module).suggestName("mem")
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
        Module(mmio.module).suggestName("mmio_mem")
      }
      mmio.io_axi4.head <> port.bits
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
            |
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
            |
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
            |   initial begin: load_data
            |       sstr_t filename = '0;
            |       if (!$$value$$plusargs("binary=%s", filename)) begin
            |           $$display("+binary=HEXFILE is required.");
            |           $$stop;
            |       end
            |       $$readmemh(filename, storage);
            |   end
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


