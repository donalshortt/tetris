package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.game._
import tetris.game.TetrisBlock
import tetris.logic.TetrisLogic._

class TetrisLogic(val randomGen: RandomGenerator,
                  val nrColumns: Int,
                  val nrRows: Int,
                  val initialBoard: Seq[Seq[TetrisBlock]]) {

  def this(random: RandomGenerator, nrColumns: Int, nrRows: Int) =
    this(random, nrColumns, nrRows, makeEmptyBoard(nrColumns, nrRows))

  def this() =
    this(new ScalaRandomGen(), DefaultWidth, DefaultHeight, makeEmptyBoard(DefaultWidth, DefaultHeight))

  val spawnPointY: Int = spawnpoint()._1
  val spawnPointX: Int = spawnpoint()._2

  val gameBoard = new GameBoard
  var currentTetromino: Tetromino = spawnTetromino()
  renderTetromino(currentTetromino)

  class GameBoard() {
    def initialiseBoard(): Array[Array[TetrisBlock]] = {
      val output = Array.ofDim[TetrisBlock](nrRows, nrColumns)

      for (i <- 0 until nrRows; j <- 0 until nrColumns) {
        output(i)(j) = initialBoard(i)(j)
      }
      output
    }

    val board: Array[Array[TetrisBlock]] = initialiseBoard()
  }

  case class Tetromino(var rotation: Int,
                       var location: Array[(Int, Int)],
                       var blockType: TetrisBlock)

  def spawnpoint(): (Int, Int) = {
    val middle = (nrColumns / 2).floor.toInt
    (SecondRowOnBoard, middle)
  }

  def spawnIBlock(): Tetromino = {
    Tetromino(
       Up,
       Array((spawnPointY, spawnPointX - 1),     // Left
             (spawnPointY, spawnPointX),         // Center-Left (Anchor)
             (spawnPointY, spawnPointX + 1),     // Center-Right
             (spawnPointY, spawnPointX + 2)),    // Right
       IBlock)
  }

  def spawnJBlock(): Tetromino = {
    Tetromino(
      Up,
      Array((spawnPointY - 1, spawnPointX - 1),  //A                                  //  +-------+
            (spawnPointY, spawnPointX - 1),      //D                                  //  | A . . |
            (spawnPointY, spawnPointX),          //E (Anchor)                         //  | D E F |
            (spawnPointY, spawnPointX + 1)),     //F                                  //  | . . . |
      JBlock)                                                                         //  +-------+
  }

  def spawnLBlock(): Tetromino = {
    Tetromino(
      Up,
      Array((spawnPointY - 1, spawnPointX + 1),   //C                                 //  +-------+
            (spawnPointY, spawnPointX - 1),       //D                                 //  | . . C |
            (spawnPointY, spawnPointX),           //E (Anchor)                        //  | D E F |
            (spawnPointY, spawnPointX + 1)),      //F                                 //  | . . . |
      LBlock)                                                                         //  +-------+
  }

  def spawnOBlock(): Tetromino = {
    Tetromino(
      Up,
      Array((spawnPointY, spawnPointX),           //D (Anchor)                        //  +-------+
            (spawnPointY - 1, spawnPointX),       //A                                 //  | A B . |
            (spawnPointY - 1, spawnPointX + 1),   //B                                 //  | D E . |
            (spawnPointY, spawnPointX + 1)),      //E                                 //  | . . . |
      OBlock)                                                                         //  +-------+
  }

  def spawnSBlock(): Tetromino = {
    Tetromino(
      Up,
      Array((spawnPointY, spawnPointX - 1),       //D                                 //  +-------+
            (spawnPointY, spawnPointX),           //E (Anchor)                        //  | . B C |
            (spawnPointY - 1, spawnPointX),       //B                                 //  | D E . |
            (spawnPointY - 1, spawnPointX + 1)),  //C                                 //  | . . . |
      SBlock)                                                                         //  +-------+
  }

  def spawnTBlock(): Tetromino = {
    Tetromino(
      Up,
      Array((spawnPointY - 1, spawnPointX),       //B                                 //  +-------+
            (spawnPointY, spawnPointX - 1),       //D                                 //  | . B . |
            (spawnPointY, spawnPointX),           //E (Anchor)                        //  | D E F |
            (spawnPointY, spawnPointX + 1)),      //F                                 //  | . . . |
      TBlock)                                                                         //  +-------+
  }

  def spawnZBlock(): Tetromino = {
    Tetromino(
      Up,
      Array((spawnPointY - 1, spawnPointX - 1),   //A                                 //  +-------+
            (spawnPointY - 1, spawnPointX),       //B                                 //  | A B . |
            (spawnPointY, spawnPointX),           //E (Anchor)                        //  | . E F |
            (spawnPointY, spawnPointX + 1)),      //F                                 //  | . . . |
      ZBlock)                                                                         //  +-------+
  }

  def renderTetromino(tetromino: Tetromino): Unit = {
    for (i <- 0 until NumberOfMiniblocks){
      gameBoard.board(tetromino.location(i)._1)(tetromino.location(i)._2) = tetromino.blockType
    }
  }

  def spawnTetromino(): Tetromino = {
    val randomNumber = randomGen.randomInt(7)

    randomNumber match {
      case 0 => spawnIBlock()
      case 1 => spawnJBlock()
      case 2 => spawnLBlock()
      case 3 => spawnOBlock()
      case 4 => spawnSBlock()
      case 5 => spawnTBlock()
      case 6 => spawnZBlock()
    }
  }

  def storePreviousLocation(): Array[(Int,Int)] = {
    val tempLocationStore: Array[(Int, Int)] = Array.ofDim(4)
    for (i <- 0 until NumberOfMiniblocks) {
      tempLocationStore(i) = (currentTetromino.location(i)._1, currentTetromino.location(i)._2)
    }
    tempLocationStore
  }

  def scrubPreviousLocation(array: Array[(Int,Int)]): Unit = {
    for (i <- 0 until NumberOfMiniblocks) {
      gameBoard.board(array(i)._1)(array(i)._2) = Empty
    }
  }

  def shiftLeft(): Unit = {
    currentTetromino = currentTetromino.copy(location = Array(
      (currentTetromino.location(0)._1, currentTetromino.location(0)._2 - 1),
      (currentTetromino.location(1)._1, currentTetromino.location(1)._2 - 1),
      (currentTetromino.location(2)._1, currentTetromino.location(2)._2 - 1),
      (currentTetromino.location(3)._1, currentTetromino.location(3)._2 - 1)))
  }

  def shiftRight(): Unit = {
//    currentTetromino = currentTetromino.copy(location = Array(
//      (currentTetromino.location(0)._1, currentTetromino.location(0)._2 + 1),
//      (currentTetromino.location(1)._1, currentTetromino.location(1)._2 + 1),
//      (currentTetromino.location(2)._1, currentTetromino.location(2)._2 + 1),
//      (currentTetromino.location(3)._1, currentTetromino.location(3)._2 + 1)))

    currentTetromino = currentTetromino.copy(currentTetromino = currentTetromino.location.map(_._2 + 1))
  }

  def shiftDown(): Unit = {
    currentTetromino = currentTetromino.copy(location = Array(
      (currentTetromino.location(0)._1 + 1, currentTetromino.location(0)._2),
      (currentTetromino.location(1)._1 + 1, currentTetromino.location(1)._2),
      (currentTetromino.location(2)._1 + 1, currentTetromino.location(2)._2),
      (currentTetromino.location(3)._1 + 1, currentTetromino.location(3)._2)))
  }

  // TODO implement me
  def rotateLeft(): Unit = ()

  // TODO implement me
  def rotateRight(): Unit = ()

  // TODO implement me
  def moveLeft(): Unit = {
    val previousLocation = storePreviousLocation()
    shiftLeft()
    scrubPreviousLocation(previousLocation)
    renderTetromino(currentTetromino)
  }

  // TODO implement me
  def moveRight(): Unit = {
    val previousLocation = storePreviousLocation()
    shiftRight()
    scrubPreviousLocation(previousLocation)
    renderTetromino(currentTetromino)
  }

  // TODO implement me
  def moveDown(): Unit = {
    val previousLocation = storePreviousLocation()
    shiftDown()
    scrubPreviousLocation(previousLocation)
    renderTetromino(currentTetromino)
  }

  // TODO implement me
  def doHardDrop(): Unit = ()

  // TODO implement me
  def isGameOver: Boolean = false

  // TODO implement me
  def getBlockAt(x: Int, y: Int): TetrisBlock = gameBoard.board(y)(x)
}

object TetrisLogic {
  def makeEmptyBoard(nrColumns: Int, nrRows: Int): Seq[Seq[TetrisBlock]] = {
    val emptyLine = Seq.fill(nrColumns)(Empty)
    Seq.fill(nrRows)(emptyLine)
  }

  val DefaultWidth: Int = 10
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines

  val SecondRowOnBoard = 1
  val NumberOfMiniblocks = 4

  val Up: Int = 0
  val Right: Int = 1
  val Down: Int = 2
  val Left: Int = 3

  def apply() = new TetrisLogic(new ScalaRandomGen(),
    DefaultWidth,
    DefaultHeight,
    makeEmptyBoard(DefaultWidth, DefaultHeight))
}