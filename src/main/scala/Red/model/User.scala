

package Red.model {

import net.liftweb._
import http.js.JsCmds.FocusOnLoad._
import http.{S, CleanRequestVarOnSessionTransition, RequestVar, SessionVar}
import mapper._
import util._
import common._
import util.AnyVarTrait._
import util.AnyVarTrait._
import util.AnyVarTrait._
import util.BindHelpers._

/**
 * The singleton that has methods for accessing the database
 */
object User extends LongKeyedMetaMapper[User] with IdPK{

  override def dbTableName = "users" // define the DB table name
  private object curUserId extends SessionVar(Empty)

  def currentUserId = curUserId.is
  protected def destroySessionOnLogin = true
  private object curUser extends RequestVar[Box[User]](find(By(Id, currentUserId.openOr(""))))  with CleanRequestVarOnSessionTransition

  def currentUser: Box[User] = curUser.is

  def loggedIn_? = {
    currentUserId.isDefined
  }

  def logUserIdIn(id: String) {
    curUser.remove()
    curUserId(Full(id))
  }

  def logUserIn(who: User, postLogin: () => Nothing): Nothing = {
    if (destroySessionOnLogin) {
      S.session.open_!.destroySessionAndContinueInNewSession(() => {
        logUserIn(who)
        postLogin()
      })
    } else {
      logUserIn(who)
      postLogin()
    }
  }

  def logUserIn(who: User) {
    curUserId.remove()
    curUser.remove()
    curUserId(Full(who.Id))
    curUser(Full(who))
  }

  def logoutCurrentUser = logUserOut()

  def logUserOut() {
    curUserId.remove()
    curUser.remove()
    S.session.foreach(_.destroySession())
  }

  def login = {
    if (S.post_?) {
      S.param("username").
        flatMap(username => findUserByUserName(username)) match {
        case Full(user) if user.validated_? &&
          user.testPassword(S.param("password")) => {
          val preLoginState = capturePreLoginState()
          val redir = loginRedirect.is match {
            case Full(url) =>
              loginRedirect(Empty)
              url
            case _ =>
              homePage
          }

          logUserIn(user, () => {
            S.notice(S.??("logged.in"))

            preLoginState()

            S.redirectTo(redir)
          })
        }

        case Full(user) if !user.validated_? =>
          S.error(S.??("account.validation.error"))

        case _ => S.error(S.??("invalid.credentials"))
      }
    }

    bind("user", loginXhtml,
      "email" -> (FocusOnLoad(<input type="text" name="username"/>)),
      "password" -> (<input type="password" name="password"/>),
      "submit" -> (<input type="submit" value={S.??("log.in")}/>))
  }
}

class User extends LongKeyedMapper[User] {
  def getSingleton = User // what's the "meta" server

  object userEmail extends MappedEmail(this)
  object password extends MappedPassword(this)

}

}