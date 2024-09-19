package br.unb.cic.oberon.codegen

import br.unb.cic.oberon.ir.ast.{Constant => ASTConstant, _}
import br.unb.cic.oberon.ir.tac._
import br.unb.cic.oberon.util.Resources
import org.scalatest.funsuite.AnyFunSuite


class RiscvCodeGenTest extends AnyFunSuite {
    test("add + rem + slt") {
      TACodeGenerator.reset
      val t0 = new Temporary(IntegerType, 11, true)
      val t1 = new Temporary(IntegerType, 12, true)
      val t2 = new Temporary(IntegerType, 13, true)
      val ops = List(
        AddOp(Constant("9", IntegerType), Constant("4", IntegerType), t0, ""),
        RemOp(t0, Constant("3", IntegerType), t1, ""),
        SLTOp(t1, t0, t2, "")
      )

      val generatedCode = RiscvCodeGenerator.generateCode(ops)
      val baseCode = Resources.getContent(s"riscvCode/add_rem_slt.asm")

      assert(generatedCode == baseCode)
    }

    test("sub + div + mul") {
      TACodeGenerator.reset
      val t0 = new Temporary(IntegerType, 8, true)
      val t1 = new Temporary(IntegerType, 9, true)
      val t2 = new Temporary(IntegerType, 10, true)
      val ops = List(
        MulOp(Constant("2", IntegerType), Constant("2", IntegerType), t0, ""),
        DivOp(t0, Constant("3", IntegerType), t1, ""),
        SubOp(t1, Constant("6", IntegerType), t2, "")
      )
      val generatedCode = RiscvCodeGenerator.generateCode(ops)
      val baseCode = Resources.getContent(s"riscvCode/sub_div_mul.asm")  

      assert(generatedCode == baseCode)
    }
    
    test("jump operation") {
      TACodeGenerator.reset
      val t0 = new Temporary(IntegerType, 8, true)
      val t1 = new Temporary(IntegerType, 9, true)
      val l1 = LabelGenerator.generateLabel.replace(":","")
      val ops = List(
        AddOp(Constant("1", IntegerType), Constant("6", IntegerType), t0, ""),
        Jump(l1, ""),
        AddOp(Constant("4", IntegerType), Constant("5", IntegerType), t0, ""),
        NOp(l1)
      )

      val generatedCode = RiscvCodeGenerator.generateCode(ops)
      val baseCode = Resources.getContent(s"riscvCode/jump.asm")  
      assert(generatedCode == baseCode)
    }
    
    
}