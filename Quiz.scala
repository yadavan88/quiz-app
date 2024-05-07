//> using scala 3
//> using dep com.lihaoyi::requests::0.8.2
//> using dep com.lihaoyi::upickle::3.2.0
//> using dep org.xerial:sqlite-jdbc:3.45.2.0
//> using dep com.lihaoyi::fansi::0.5.0

import upickle.default.{ReadWriter => RW, macroRW}
import upickle.*, upickle.default.*
import scala.util.{Try, Using}
import scala.util.Random
import scala.io.StdIn.* 
import scala.util.Success
import scala.util.Failure
import fansi.Color.*

case class Question(
    `type`: String,
    question: String,
    correct_answer: String,
    incorrect_answers: List[String]
) {
  val allOptions: List[String] = Random.shuffle(correct_answer :: incorrect_answers)
}
object Question {
  given ReadWriter[Question] = macroRW
}

def getQuestions(): List[Question] = {
  val questionCount = 50
  val response = requests.get(
    s"https://opentdb.com/api.php?amount=${questionCount}&category=9&difficulty=easy&type=multiple"
  )
  val fullResponse = response.text()
  val questionsJson = ujson.read(fullResponse)("results")
  val questions = read[List[Question]](questionsJson)

  questions.toList
}

@main
def main(): Unit = {
  val questions = getQuestions()
  val resetQuestions = true
  if(resetQuestions) {
    QuizDAO.createEmptyDB()
    println("--------------------------------------------")
    QuizDAO.saveQuestions(questions)
    println(s"saved ${questions.size} questions to DB.. ")
    println("--------------------------------------------")
  }
  
  val questionsFromDB = QuizDAO.getQuestions()
  // questionsFromDB.foreach(println)
  questionsFromDB match {
    case Success(questions) => 
      println(s"Retrieved ${questions.size} questions from DB..")
      while (true)
        askRandomQuestion(questions)
    case Failure(exception) => println("Error loading questions from DB" + exception)}
  
}

def askRandomQuestion(questions: Seq[Question]): Unit = {
  val question = Random.shuffle(questions).head
  println(question.question)
  question.allOptions.zipWithIndex.foreach { case (option, index) =>
    println(s"${index + 1}. $option")
  }
  print("Your answer: ")
  val answer = readInt()
  
  println()
  if (question.allOptions(answer - 1) == question.correct_answer) {
    println(Green("Correct!" + question.correct_answer))
  } else {
    println(Red("Incorrect!"))
    println(s"The correct answer is: ${question.correct_answer}")
  }
}

object QuizDAO {
  val url = "jdbc:sqlite:./Quiz.sqlite"

  def createEmptyDB() = {
    val conn = java.sql.DriverManager.getConnection(url)
    val stmt = conn.createStatement()
    stmt.executeUpdate("DROP TABLE IF EXISTS questions")
    stmt.executeUpdate(
      "CREATE TABLE questions (type VARCHAR(25), question TEXT, correct_answer VARCHAR(100), incorrect_answers TEXT)"
    )
    conn.close()
  }

  def saveQuestions(questions: Seq[Question]) = {
    val conn = java.sql.DriverManager.getConnection(url)
    val stmt = conn.createStatement()

    val prep = conn.prepareStatement(
      "INSERT INTO questions (type, question, correct_answer, incorrect_answers) VALUES (?, ?, ?, ?)"
    )
    questions.foreach { q =>
      prep.setString(1, q.`type`)
      prep.setString(2, q.question)
      prep.setString(3, q.correct_answer)
      prep.setString(4, q.incorrect_answers.mkString(","))
      prep.executeUpdate()
    }
    conn.close()
  }

  // using the simple java way to read from sqlite and ignoring proper error handling for now
  def getQuestions(): Try[List[Question]] = Try {
    val conn = java.sql.DriverManager.getConnection(url)
    val stmt = conn.createStatement()
    val rs = stmt.executeQuery("SELECT * FROM questions")

    val questions = Iterator
      .continually(rs)
      .takeWhile(_.next())
      .map { rs =>
        Question(
          rs.getString("type"),
          rs.getString("question"),
          rs.getString("correct_answer"),
          rs.getString("incorrect_answers").split(",").toList
        )
      }
      .toList

    conn.close()

    questions
  }

}
