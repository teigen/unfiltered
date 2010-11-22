package unfiltered.oauth

import unfiltered.response._
import unfiltered.request.{HttpRequest => Req}

/** minimal user identity */
trait UserLike {
  val id: String
}

trait UserHost extends OAuthTemplates {
  
  /** @return Some(user) if user is logged in, None otherwise */
  def current[T](r: Req[T]): Option[UserLike]
  
  /** @return true if app logic determines this request was accepted, false otherwise */
  def accepted[T](token: String, r: Req[T]): Boolean

  /** @return true if app logic determines this request was denied, false otherwise */
  def denied[T](token: String, r: Req[T]): Boolean
  
  /** return the html to display to the user to log in */
  def login(token: String): unfiltered.response.Html 
  
  def deniedConfirmation = layout(
    <div>You have denied a 3rd party access to your data</div>
  )
  
  /** @todo more flexibilty wrt exensibility */
  def requestAcceptance(token: String) = layout(
    <div>
      <p>
        A 3rd party application has requested access to your data.
      </p>
      <form action={"/authorize?%s" format(token)} method="POST">
        <input type="hidden" name="oauth_token" value={token} />
        <input type="submit" value="Approve"/>
        <input type="submit" value="Deny"/>
      </form>
    </div>)
  
  /** @todo more flexibilty wrt exensibility */
  def accessDenied = layout(
    <div>
      <p>
        You have denied this 3rd party application access to your data.
      </p>
    </div>)
}
