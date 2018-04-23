package TimeExp

import java.time.{DayOfWeek, LocalDate, MonthDay, YearMonth}
import java.time.temporal.ChronoUnit.{DAYS, MONTHS, YEARS}
import java.time.temporal.WeekFields

/*
for the type, to avoid int,... we could use: https://github.com/fthomas/refined
or we can use tagged types
 */

object TimeExpression {


  private[TimeExp] class genericTimeExpression( howToEvaluate: LocalDate=>  Boolean) extends TimeExpression{
    def isRecurringOn(localDate: LocalDate): Boolean = howToEvaluate(localDate)
  }

  private[TimeExp] def evalDiffDay( from: LocalDate, local: LocalDate):Long =
    DAYS.between(from, local)

  private[TimeExp] def isFirstDayOfTheMonth( localDate: LocalDate) =
    localDate.getDayOfMonth < 7


  private[TimeExp] def isLastDayOfTheMonth( localDate: LocalDate) =
    localDate.getDayOfMonth +7 > localDate.lengthOfMonth


  private[TimeExp] def getWeekOfTheMonth( from: LocalDate):Int =
    from.get(WeekFields.ISO.weekOfMonth())


  /**
    * This expression matches on the date of parameter value.
    *
    * @param localDate a local date
    * @return a TimeExpression
    */
  def apply(from: LocalDate): TimeExpression ={
    new genericTimeExpression( local => evalDiffDay(from,local) == 0)
  }

  def daily(every: Int, from: LocalDate): TimeExpression =
    new genericTimeExpression( local => evalDiffDay(from,local) % every == 0)


  def monthlyEvery(amountOfMonth: Int, dayOfMonth: Int, fromYearMonth: YearMonth): TimeExpression = {
    val from = fromYearMonth.atDay(dayOfMonth)
    new genericTimeExpression(
      local => from.getDayOfMonth == local.getDayOfMonth
        && MONTHS.between(local, from) % amountOfMonth == 0
        && fromYearMonth.minusMonths(1).atEndOfMonth().isBefore(local)
    )
  }


  def monthlyEvery(amountOfMonth: Int, dayOfWeek: DayOfWeek, weekOfMonth: Int, fromYearMonth: YearMonth): TimeExpression = {
    require( weekOfMonth ==1 || weekOfMonth >=4 )//accept only first or last week . better use boolean for next version.

    val from = fromYearMonth.atEndOfMonth()

    val evalShared = (local:LocalDate) =>
      dayOfWeek == local.getDayOfWeek && MONTHS.between(local, from) % amountOfMonth == 0 &&
        local.getYear >= fromYearMonth.getYear

    if( weekOfMonth ==1  )
      new genericTimeExpression( local =>  evalShared(local) && isFirstDayOfTheMonth(local))
    else
      new genericTimeExpression( local =>  evalShared(local) && isLastDayOfTheMonth(local))
  }

  def yearlyEvery(amountOfYears: Int, day: MonthDay, fromYear: Int): TimeExpression = {
    new genericTimeExpression( local => (local.getYear - fromYear) % amountOfYears ==0  &&
      local.getYear >= fromYear &&
      day.getDayOfMonth == local.getDayOfMonth
    )
  }
}


trait TimeExpression {
  def isRecurringOn(localDate: LocalDate): Boolean
}
