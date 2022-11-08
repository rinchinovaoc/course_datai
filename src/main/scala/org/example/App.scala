package org.example
import scala.io.StdIn
import java.io._
/**
 * @author ${user.name}
 */
//a. Напишите программу, которая:
//i.выводит фразу «Hello, Scala !» справа налево
//ii.переводит всю фразу в нижний регистр
//iii.удаляет символ !
//iv.добавляет в конец фразы «and goodbye python!»
object App {
  def Task1(str_h: String): Unit =
  {
    println(str_h.reverse)
    println(str_h.toLowerCase())
    println(str_h.replace("!", ""))
    println(str_h.replace("!", "") + " and goodbye python!")
  }

  //b. Напишите программу, которая вычисляет ежемесячный оклад сотрудника после вычета налогов.
  //На вход вашей программе подается значение годового дохода до вычета налогов,
  //размер премии – в процентах от годового дохода и компенсация питания.
  def salaryMonth(revenue: Int, bonus: Float, foodCompensation: Int): Int = ((revenue*(1 + bonus) + foodCompensation)*0.87/12).floatValue.round
  //с. Напишите программу, которая рассчитывает для каждого сотрудника отклонение(в процентах) от среднего значения оклада на уровень всего отдела.
  //В итоговом значении должно учитываться в большую или меньшую сторону отклоняется размер оклада.
  //На вход вышей программе подаются все значения, аналогичные предыдущей программе,
  //а также список со значениями окладов сотрудников отдела 100, 150, 200, 80, 120, 75.
  def Task3(revenue: Int, bonus: Float, foodCompensation: Int, salariesList: List[Int]): Double = (1 - salaryMonth(revenue, bonus, foodCompensation)/middleSalary(salariesList))
  def middleSalary(salariesList: List[Int]): Float = salariesList.sum/salariesList.size
  //d. Попробуйте рассчитать новую зарплату сотрудника, добавив(или отняв, если сотрудник плохо себя вел) необходимую сумму с учетом результатов прошлого задания.
  // Добавьте его зарплату в список и вычислите значение самой высокой зарплаты и самой низкой.
  def addNewSalaryToList(salariesList: List[Int], addToSalary: Int): List[Int] = salariesList :+ addToSalary
  //e. Также в вашу команду пришли два специалиста с окладами 350 и 90 тысяч рублей.
  //Попробуйте отсортировать список сотрудников по уровню оклада от меньшего к большему.
  def sortListSalaries(salariesList: List[Int]): List[Int] = salariesList.sorted
  //f. Кажется, вы взяли в вашу команду еще одного сотрудника и предложили ему оклад 130 тысяч.
  //Вычислите самостоятельно номер сотрудника в списке так, чтобы сортировка не нарушилась и добавьте его на это место.
  def addNewSalaryToListInSort(salariesList: List[Int], addToSalary: Int): List[Int] =
    {
      var newSalariesList: List[Int] = List()
      var addElement: Boolean = false
      for (n <- 0 until salariesList.size)
        {
          if ((salariesList(n) > addToSalary) && (addElement == false))
            {
              newSalariesList = addNewSalaryToList(newSalariesList, addToSalary)
              addElement = true
            }
          newSalariesList = addNewSalaryToList(newSalariesList, salariesList(n))
        }
        return newSalariesList
    }
  //g. Попробуйте вывести номера сотрудников из полученного списка, которые попадают под категорию middle.
  //На входе программе подается «вилка» зарплаты специалистов уровня middle.
  def printMiddleSalaries(salariesList: List[Int], sMin: Int, sMax: Int): Unit =
  {
    println("Зарплаты уровня middle: ")
    for (n <- 0 until salariesList.size) {
      if ((salariesList(n) >= sMin) && (salariesList(n) <= sMax)) {
        println(salariesList(n))
      }
    }
  }
  //h. Однако наступил кризис и ваши сотрудники требуют повысить зарплату.
  //Вам необходимо проиндексировать зарплату каждого сотрудника на уровень инфляции – 7%
  def IndexSalary(salariesList: List[Int], persentInflation: Double): List[Int] = {
    var newSalariesList: List[Int] = List()
    for (n <- 0 until salariesList.size) {
      newSalariesList = addNewSalaryToList(newSalariesList, (salariesList(n)*(1+persentInflation)).toInt)
    }
    return newSalariesList
  }
  //i.*Ваши сотрудники остались недовольны и просят индексацию на уровень рынка.
  //Попробуйте повторить ту же операцию, как и в предыдущем задании,
  //но теперь вам нужно проиндексировать зарплаты на процент отклонения от среднего по рынку с учетом уровня специалиста.
  //На вход вашей программе подается 3 значения – среднее значение зарплаты на рынке для каждого уровня специалистов(junior, middle и senior)
  def IndexSalaryMiddle(salariesList: List[Int], middleJunior: Int, middleMiddle: Int, middleSenior: Int, sMinMiddle: Int, sMaxMiddle: Int): List[Int] = {
    var newSalariesList: List[Int] = List()
    for (n <- 0 until salariesList.size) {
      if (salariesList(n) <= sMinMiddle) {
        newSalariesList = addNewSalaryToList(newSalariesList, middleJunior)
      }
      else if (salariesList(n) <= sMaxMiddle) {
        newSalariesList = addNewSalaryToList(newSalariesList, middleMiddle)
      }
      else {
        newSalariesList = addNewSalaryToList(newSalariesList, middleSenior)
      }
    }
    return newSalariesList
  }

  //k.*Попробуйте деанонимизировать ваших сотрудников – составьте структуру, которая позволит иметь знания о том,
  // сколько зарабатывает каждый сотрудник(Фамилия и имя).
  class Employer(var nameEmployer: String, var salary: Int){
    def printData: Unit ={
      print("ФИО: ")
      println(nameEmployer)
      print("Зарплата: ")
      println(salary)
    }
  }
  def fillEmployerList1(): List[Employer] = {
    var newEmployerList: List[Employer] = List()
    newEmployerList = newEmployerList :+ new Employer("Rinchinova Oyuna", 150)
    newEmployerList = newEmployerList :+ new Employer("Rinchinova Olga", 100)
    newEmployerList = newEmployerList :+ new Employer("Rinchinov Roman", 190)
    newEmployerList = newEmployerList :+ new Employer("Ivanov Ivan", 90)
    newEmployerList = newEmployerList :+ new Employer("Komuzakin Valeri", 250)
    newEmployerList = newEmployerList :+ new Employer("UTOT Paren", 140)
    newEmployerList = newEmployerList :+ new Employer("Toot Par", 95)
    return newEmployerList
  }
  def sortBySalary(s1: Employer, s2: Employer) = {
    s1.salary > s2.salary
  }
  def main(args : Array[String]): Unit = {
    //
    Task1("Hello, Scala!")
    //
    println("Введите годовой доход: ")
    var revenue = readLine().toInt
    println("Введите размер премии - в процентах (от 0 до 1): ")
    var bonus = readLine().toFloat
    println("Введите компенсацию питания: ")
    var foodCompensation = readLine().toInt
    var salaryMonthEmp = salaryMonth(revenue, bonus, foodCompensation)
    println("ежемесячный оклад сотрудника: "+salaryMonthEmp)
    //
    var salariesList = List(100, 150, 200, 80, 120, 75)
    println("средний оклад сотрудника: "+middleSalary(salariesList))
    //
    var percentSalary = Task3(revenue, bonus, foodCompensation, salariesList)
    if (percentSalary > 0)
    {
      println(f"ежемесячный оклад сотрудника меньше на : $percentSalary процентов")
    }
    else
    {
      percentSalary = -1 * percentSalary
      println(f"ежемесячный оклад сотрудника больше на : $percentSalary процентов")
    }
    //
    println("Введите сумму для добавления к зарплате сотрудника: ")
    var addToSalary = readLine().toInt
    salariesList = addNewSalaryToList(salariesList, salaryMonthEmp + addToSalary)
    println(salariesList)
    //
    salariesList = addNewSalaryToList(salariesList, 350)
    salariesList = addNewSalaryToList(salariesList, 90)
    salariesList = sortListSalaries(salariesList)
    println("Список зарплат сотрудников, отсортированный по возрастанию, после прихода новых специалистов: ")
    println(salariesList)
    //
    salariesList = addNewSalaryToListInSort(salariesList, 130)
    println("Список зарплат сотрудников, отсортированный по возрастанию, после прихода нового специалиста с зарплатой 130: ")
    println(salariesList)
    //
    println("Введите минимум для middle: ")
    var sMin = readLine().toInt
    println("Введите максимум для middle: ")
    var sMax = readLine().toInt
    printMiddleSalaries(salariesList, sMin, sMax)
    //
    salariesList = IndexSalary(salariesList: List[Int], 0.07)
    println("Список проиндексированных на 7 процентов зарплат сотрудников")
    println(salariesList)
    //
    println("Введите среднее значение зарплаты на рынке для специалистов уровня junior: ")
    var middleJunior = readLine().toInt
    println("Введите среднее значение зарплаты на рынке для специалистов уровня middle: ")
    var middleMiddle = readLine().toInt
    println("Введите среднее значение зарплаты на рынке для специалистов уровня senior: ")
    var middleSenior = readLine().toInt
    //Введите среднее значение зарплаты на рынке для специалистов уровня junior, middle и senior
    salariesList = IndexSalaryMiddle(salariesList, middleJunior, middleMiddle, middleSenior, sMin, sMax)
    println("Список проиндексированных на средний уровень зарплат сотрудников")
    println(salariesList)
    //
    var employersList = fillEmployerList1()
    employersList = employersList.sortWith(sortBySalary)
    //l.*Выведите фамилию и имя сотрудников с самой высокой и самой низкой зарплатой(только не рассказывайте им об этом факте).
    println("сотрудник с самой высокой зарплатой: ")
    employersList(0).printData
    println("сотрудник с самой низкой зарплатой: ")
    employersList(employersList.size-1).printData
  }
}
