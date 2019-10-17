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

  //TEMP
  def printBoard(): Unit = {
    val output = Array.ofDim[Int](nrRows, nrColumns)

    for (i <- 0 until nrRows; j <- 0 until nrColumns) {
      gameBoard.board(i)(j) match {
        case Empty => output(i)(j) = 0
        case IBlock => output(i)(j) = 1
        case JBlock => output(i)(j) = 2
        case LBlock => output(i)(j) = 3
        case OBlock => output(i)(j) = 4
        case SBlock => output(i)(j) = 5
        case TBlock => output(i)(j) = 6
        case ZBlock => output(i)(j) = 7
      }
    }

    for (i <- 0 until nrRows) {
      for (j <- 0 until nrColumns) {
        printf(output(i)(j).toString + " ")
      }
      printf("[" + i + "]\n")
    }
  }


  case class Tetromino(var rotation: Int,
                       var location: Array[(Int, Int)],
                       var blockType: TetrisBlock)

  def clearLine(lineIndex: Int): Unit = { gameBoard.board(lineIndex) = Array.fill[TetrisBlock](nrColumns)(Empty) }

  def shiftBoardDown(lineIndex: Int): Unit = {
    for (i <- lineIndex until 0 by -1){
      gameBoard.board(i) = gameBoard.board(i - 1)
    }
  }

  def checkForLine(): Unit = {
    for (i <- nrRows - 1 until 0 by -1){
      if(!(gameBoard.board(i) contains Empty)) {
        printf("<!>PRINT BOARD<!> - Pre - CheckForLine\n")
        printBoard()
        clearLine(i)
        shiftBoardDown(i)
        checkForLine()
        printf("<!>PRINT BOARD<!> - After - CheckForLine\n")
        printBoard()
      }
    }
  }

  def isEven(i: Int): Boolean = {i % 2 == 0}

  def spawnpoint(): (Int, Int) = {
    var middle = (nrColumns / 2)
    if (isEven(nrColumns)) middle -= 1
    (SecondRowOnBoard, middle)
  }

  def spawnIBlock(): Tetromino = {
    Tetromino(
       Up,
       Array((spawnPointY, spawnPointX),         // Center-Left (Anchor)
             (spawnPointY, spawnPointX - 1),     // Left
             (spawnPointY, spawnPointX + 1),     // Center-Right
             (spawnPointY, spawnPointX + 2)),    // Right
       IBlock)
  }

  def spawnJBlock(): Tetromino = {
    Tetromino(
      Up,
      Array((spawnPointY, spawnPointX),          //E (Anchor)                         //  +-------+
            (spawnPointY, spawnPointX - 1),      //D                                  //  | A . . |
            (spawnPointY - 1, spawnPointX - 1),  //A                                  //  | D E F |
            (spawnPointY, spawnPointX + 1)),     //F                                  //  | . . . |
      JBlock)                                                                         //  +-------+
  }

  def spawnLBlock(): Tetromino = {
    Tetromino(
      Up,
      Array((spawnPointY, spawnPointX),           //E (Anchor)                        //  +-------+
            (spawnPointY, spawnPointX - 1),       //D                                 //  | . . C |
            (spawnPointY - 1, spawnPointX + 1),   //C                                 //  | D E F |
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
      Array(
            (spawnPointY, spawnPointX),           //E (Anchor)                        //  +-------+
            (spawnPointY, spawnPointX - 1),       //D                                 //  | . B C |
            (spawnPointY - 1, spawnPointX),       //B                                 //  | D E . |
            (spawnPointY - 1, spawnPointX + 1)),  //C                                 //  | . . . |
      SBlock)                                                                         //  +-------+
  }

  def spawnTBlock(): Tetromino = {
    Tetromino(
      Up,
      Array((spawnPointY, spawnPointX),           //E (Anchor)                        //  +-------+
            (spawnPointY, spawnPointX - 1),       //D                                 //  | . B . |
            (spawnPointY - 1, spawnPointX),       //B                                 //  | D E F |
            (spawnPointY, spawnPointX + 1)),      //F                                 //  | . . . |
      TBlock)                                                                         //  +-------+
  }

  def spawnZBlock(): Tetromino = {
    Tetromino(
      Up,
      Array((spawnPointY , spawnPointX ),         //E (Anchor)                        //  +-------+
            (spawnPointY - 1, spawnPointX),       //B                                 //  | A B . |
            (spawnPointY - 1, spawnPointX - 1),   //A                                 //  | . E F |
            (spawnPointY, spawnPointX + 1)),      //F                                 //  | . . . |
      ZBlock)                                                                         //  +-------+
  }

  def renderTetromino(tetromino: Tetromino): Unit = {
    for (i <- 0 until NumberOfMiniblocks){ gameBoard.board(tetromino.location(i)._1)(tetromino.location(i)._2) = tetromino.blockType }
    printf("Rendered tetro!!\n")
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
    val tempLocationStore: Array[(Int, Int)] = Array.ofDim(NumberOfMiniblocks)
    for (i <- 0 until NumberOfMiniblocks) {
      tempLocationStore(i) = (currentTetromino.location(i)._1, currentTetromino.location(i)._2)
    }
    tempLocationStore
  }

  def scrubLocation(location: Array[(Int,Int)]): Unit = {
    for (i <- 0 until NumberOfMiniblocks) {
      gameBoard.board(location(i)._1)(location(i)._2) = Empty
    }
  }

  def restoreLocation(location: Array[(Int, Int)]): Unit = {
    for (i <- 0 until NumberOfMiniblocks) {
      gameBoard.board(location(i)._1)(location(i)._2) = currentTetromino.blockType
    }
  }

  def rotateTetroLeft(input: (Int, Int)): (Int, Int) = {
    var offset = (0,0)
    val anchor = currentTetromino.location(0)
    def rotate(input: (Int, Int)): (Int, Int) = (-input._2, input._1)
    if(currentTetromino.blockType == IBlock){
      offset = (currentTetromino.location(1)._1-anchor._1, currentTetromino.location(1)._2-anchor._2)
      offset = rotate(offset)
    }
    val newY = -(input._2 - anchor._2) + anchor._1 + offset._1
    val newX = (input._1 - anchor._1) + anchor._2 + offset._2
    (newY, newX)
  }

  def rotateTetroRight(input: (Int, Int)): (Int, Int) = {
    var offset = (0,0)
    val anchor = currentTetromino.location(0)
    if(currentTetromino.blockType == IBlock){
      offset = (currentTetromino.location(1)._1-anchor._1, currentTetromino.location(1)._2-anchor._2)
    }
    val newY = (input._2 - anchor._2) + anchor._1 - offset._1
    val newX = -(input._1 - anchor._1) + anchor._2 - offset._2
    (newY, newX)
  }

  def rotateTetromino(direction: Int): Unit = {
    direction match {
      case Left => currentTetromino = currentTetromino.copy(location = currentTetromino.location map(rotateTetroLeft))
      case Right => currentTetromino = currentTetromino.copy(location = currentTetromino.location map(rotateTetroRight))
    }
  }

  def shiftTetroRight(input: (Int, Int)): (Int, Int) = { (input._1, input._2 + 1) }

  def shiftTetroLeft(input: (Int, Int)): (Int, Int) = { (input._1, input._2 - 1) }

  def shiftTetroDown(input: (Int, Int)): (Int, Int) = { (input._1 + 1, input._2) }

  def shiftTetromino(direction: Int): Unit = {
    direction match {
      case Right => currentTetromino = currentTetromino.copy(location = currentTetromino.location map(shiftTetroRight))
      case Left => currentTetromino = currentTetromino.copy(location = currentTetromino.location map(shiftTetroLeft))
      case Down => currentTetromino = currentTetromino.copy(location = currentTetromino.location map(shiftTetroDown))
    }
  }

  def generateProposedLocation(prevLoc: Array[(Int, Int)], direction: Int, moveType: Int): Array[(Int, Int)] = {
    var proposedLocation = new Array[(Int, Int)](NumberOfMiniblocks)

    if(moveType == Shift)
      direction match {
        case Left => proposedLocation = prevLoc map (shiftTetroLeft)
        case Right => proposedLocation = prevLoc map (shiftTetroRight)
        case Down => proposedLocation = prevLoc map (shiftTetroDown)
      } else {
      direction match {
        case Left => proposedLocation = prevLoc map (rotateTetroLeft)
        case Right => proposedLocation = prevLoc map (rotateTetroRight)
      }
    }
    proposedLocation
  }

  def moveIsValid(prevLoc: Array[(Int, Int)], direction: Int, moveType: Int): Boolean = {
    var proposedLocation = new Array[(Int, Int)](NumberOfMiniblocks)

    proposedLocation = generateProposedLocation(prevLoc, direction, moveType)

    for (i <- 0 until NumberOfMiniblocks) {
      if(proposedLocation(i)._1 >= nrRows) { return false }
      if(proposedLocation(i)._2 >= nrColumns || proposedLocation(i)._2 < 0) { return false }
      if(gameBoard.board(proposedLocation(i)._1)(proposedLocation(i)._2) != Empty) { return false }
    }
    true
  }

  def rotateLeft(): Unit = {
    if (currentTetromino.blockType == OBlock) return
    val previousLocation = storePreviousLocation()
    scrubLocation(previousLocation)

    if(moveIsValid(previousLocation, Left, Rotation)) {
      rotateTetromino(Left)
    } else { restoreLocation(previousLocation) }

    renderTetromino(currentTetromino)
  }

  def rotateRight(): Unit = {
    if (currentTetromino.blockType == OBlock) return
    val previousLocation = storePreviousLocation()
    scrubLocation(previousLocation)

    if(moveIsValid(previousLocation, Right, Rotation)) {
      rotateTetromino(Right)
    } else { restoreLocation(previousLocation) }

    renderTetromino(currentTetromino)
  }

  def moveLeft(): Unit = {
    val previousLocation = storePreviousLocation()
    scrubLocation(previousLocation)

    if(moveIsValid(previousLocation, Left, Shift)) {
      shiftTetromino(Left)
    } else { restoreLocation(previousLocation) }

    renderTetromino(currentTetromino)
  }

  def moveRight(): Unit = {
    val previousLocation = storePreviousLocation()
    scrubLocation(previousLocation)

    if(moveIsValid(previousLocation, Right, Shift)) {
      shiftTetromino(Right)
    } else { restoreLocation(previousLocation) }

    renderTetromino(currentTetromino)
  }

  def moveDown(): Unit = {
    val previousLocation = storePreviousLocation()
    scrubLocation(previousLocation)

    if(moveIsValid(previousLocation, Down, Shift)) {
      shiftTetromino(Down)
    } else {
      restoreLocation(previousLocation)
      checkForLine()
      currentTetromino = spawnTetromino()
    }

    renderTetromino(currentTetromino)
    printf("<!>PRINT BOARD<!> - After - Render\n")
    printf("CurrentTetro Size: " + currentTetromino.location.size + "\n")
    printBoard()
  }

  // TODO implement me
  def doHardDrop(): Unit = ()

  // TODO implement me
  def isGameOver: Boolean = false

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

  val Shift: Int = 1
  val Rotation: Int = 2

  def apply() = new TetrisLogic(new ScalaRandomGen(),
    DefaultWidth,
    DefaultHeight,
    makeEmptyBoard(DefaultWidth, DefaultHeight))
}