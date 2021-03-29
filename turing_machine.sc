import scala.collection.mutable

type Cell = Char
type Tape = List[Cell]
type State = String
// 0: no movement, 1: right, -1: left
type Movement = Int
// (initial_state, initial_read_cell) -> (final_state, final_write_cell, movement)
type Instructions = mutable.HashMap[(State, Cell),(State, Cell, Movement)]
type HeadPosition = Int


/**
 * Scala Instructive Turing Machine;
 * Inspired on book Principles Of Quantum Computation 1 (Giuliano Benenti, Giulio Casati, Giuliano Strini)
 * @param initial_tape
 * @param _instructions
 * @param _state
 */
class TComputer(var initial_tape: Tape = List('b'),
                var _instructions: Instructions = mutable.HashMap(("s0", 'b')->("H", 'b', 0)),
                var _state: State = "s0",
                var _initial_head_position: HeadPosition = 0) {

  var tape: List[Char] = initial_tape
  var head_position: HeadPosition = _initial_head_position
  var state: State = _state
  var instructions: Instructions = _instructions
  var machine_name: String = "Custom Turing Machine"

  override def toString: String = {
    val tape_string: String = tape.mkString
    val head_string: String = List.fill(tape.length)(" ").updated(head_position, "^").mkString
    tape_string + "\n" + head_string
  }

  def read_current_cell(): Char = tape(head_position)

  def move(movement: Movement): Unit = {
    movement match {
      case 0 => Nil
      case 1 => head_position += 1
      case -1 => head_position -= 1
    }
  }

  def start_machine(): Unit = {
    println(s"Starting Turing machine: $machine_name")
    println("Initial step:")
    println(toString())
    val cell = read_current_cell()
    println(s"state: $state, cell: $cell")

    def apply_instruction(): Unit = {
      state match {
        case "H" => println("Machine halted")
        case _ => {
          println()
          println()
          println("New step: ")
          println(toString())
          val cell = read_current_cell()
          println(s"state: $state, cell read: $cell, position: $head_position")
          val (new_state, new_cell, next_movement) = instructions((state, read_current_cell()))
          state = new_state
          tape = tape.updated(head_position, new_cell)
          move(next_movement)
          println(toString())
          println(s"state: $new_state, cell wrote: $new_cell, position: $head_position")
          apply_instruction()
        }
      }
    }

    apply_instruction()
  }
}

// Create computer
val initial_tape: Tape = List('b', 'b', 'b', '1', '1', 'b', '1', 'b')
val my_instructions: Instructions = mutable.HashMap(
  ("s1", 'b') -> ("s2", 'b', -1),
  ("s2", 'b') -> ("s3", 'b', -1),
  ("s2", '1') -> ("s2", '1', -1),
  ("s3", 'b') -> ("H", 'b', 0),
  ("s3", '1') -> ("s4", 'b', 1),
  ("s4", 'b') -> ("s2", '1', -1),
)
val initial_state: State = "s1"
val initial_head_position: HeadPosition = initial_tape.length - 1
val machine_name: String = "Unary-Sum"
val my_computer = new TComputer(initial_tape, my_instructions,
                                initial_state, initial_head_position)

// Prepare machine
my_computer.machine_name = machine_name

// Get intermediate steps
my_computer.start_machine()
