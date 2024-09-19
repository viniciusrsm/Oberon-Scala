package br.unb.cic.oberon.codegen

import br.unb.cic.oberon.ir.ast.IntegerType
import br.unb.cic.oberon.ir.ast.Statement
import br.unb.cic.oberon.ir.tac._
import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc._

object RiscvCodeGenerator extends CodeGenerator[List[TAC], String] {
    val indentSize: Int = 4
    val twoLines: Doc = line * 2

    override def generateCode(module: List[TAC]): String = {
        val mainHeader =
            text(".data") / text(".text") + twoLines

        val textContent = text(module.map(generateOps).mkString)

        return (mainHeader + textContent).render(60)
    
    }

    def addrToReg(addr: Address): String = {
        addr match {
            case Temporary(t, num, manual) => {
                s"x$num"
            }
        }
    }

    def generateBinExp(addrLeft: Address, addrRight: Address, addrDest: Address, instr: String): String = {
        (addrLeft, addrRight) match {
            case (Constant(valueLeft, _), Constant(valueRight, _)) => 
                s"li x5,${valueLeft}\n" +
                s"li x6,${valueRight}\n" +
                s"${instr} ${addrToReg(addrDest)},x5,x6\n"
            case (Constant(valueLeft, _), Temporary(_, num, _)) => 
                s"li x5,${valueLeft}\n" +
                s"${instr} ${addrToReg(addrDest)},x5,x$num\n"
            case (Temporary(_, num, _), Constant(valueRight, _)) =>
                s"li x5,${valueRight}\n" +
                s"${instr} ${addrToReg(addrDest)},x$num,x5\n"
            case (Temporary(_, numLeft, _), Temporary(_, numRight, _)) => 
                s"${instr} ${addrToReg(addrDest)},x$numLeft,x${numRight}\n"
            case _ => throw new Exception("invalid binary expression")
        }
        
    }

    def generateOps(op: TAC): String = {
        op match {
            case AddOp(s1, s2, dest, label) =>  generateBinExp(s1, s2, dest, "add")
            case SubOp(s1, s2, dest, label) =>  generateBinExp(s1, s2, dest, "sub")
            case MulOp(s1, s2, dest, label) =>  generateBinExp(s1, s2, dest, "mul")
            case DivOp(s1, s2, dest, label) =>  generateBinExp(s1, s2, dest, "div")
            case RemOp(s1, s2, dest, label) =>  generateBinExp(s1, s2, dest, "rem")
            case AndOp(s1, s2, dest, label) =>  generateBinExp(s1, s2, dest, "and")
            case OrOp(s1, s2, dest, label) =>   generateBinExp(s1, s2, dest, "or")
            case SLTOp(s1, s2, dest, label) =>  generateBinExp(s1, s2, dest, "slt")
            case SLTUOp(s1, s2, dest, label) => generateBinExp(s1, s2, dest, "sltu")
            case NOp(label) => s"$label:\n"
            case Jump(destLabel, label) => s"j $destLabel\n"
            case JumpFalse(s1, destLabel, label) => s"beq $s1,x0,$destLabel"
            case JumpTrue(s1, destLabel, label) => s"bne $s1,x0,$destLabel"
            case LTEJump(s1, s2, destLabel, label) => s"ble $s1,$s2,$destLabel"
            case LTJump(s1, s2, destLabel, label) => s"blt $s1,$s2,$destLabel"
            case GTEJump(s1, s2, destLabel, label) => s"bge $s1,$s2,$destLabel"
            case GTJump(s1, s2, destLabel, label) => s"bgt $s1,$s2,$destLabel"
            case EqJump(s1, s2, destLabel, label) => s"beq $s1,$s2,$destLabel"
            case NeqJump(s1, s2, destLabel, label) => s"bne $s1,$s2,$destLabel"
        }
    }
}
