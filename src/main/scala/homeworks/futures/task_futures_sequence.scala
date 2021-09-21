package homeworks.futures

import homeworks.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */

  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    val initFuture = Future.successful((List.empty[A], List.empty[Throwable]))
    futures.foldLeft(initFuture)((resultFuture, future) =>
      resultFuture.zipWith(future.transform(Success(_))) {
        case ((values, exceptions), Success(v)) => (values ::: List(v), exceptions)
        case ((values, exceptions), Failure(ex)) => (values, exceptions ::: List(ex))
      }
    )
  }
}
