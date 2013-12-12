package taf.db

import net.liftweb.mapper.{ConnectionIdentifier, ConnectionManager, Schemifier}
import java.sql.{Connection, DriverManager}
import net.liftweb.common.{Box, Empty, Full}
import net.liftweb.mapper._


case class TestMapper() {

  def dodb : Unit =
  {
      println ("create post")
      val myPost: Post = Post.create
      myPost.title("My First Blog Post")
      // MappedFields return the model instance, so we can chain assignments
      myPost.contents("This is a very short blog post but it will have to do for now.").published(true)

      println ("activating connection manager")
      DB.defineConnectionManager(DefaultConnectionIdentifier, myDBVendor)

      println ("last but not least the Schemifier")
      Schemifier.schemify(true, Schemifier.infoF _, Post)
      println ("and save it")
      val saved: Boolean = myPost.save
      println ("finished with save " + saved)

      println ("query all records")
      val posts: List[Post] = Post.findAll
      println ("the result: " + posts)
      println ("query the title only")
      val postsTitles: List[Post] = Post.findAllFields(Seq[SelectableField](Post.title))
      println ("the titles: " + postsTitles)
      println ("does not work, does it?")

      println ("query all published rows")
      val publishedPosts: List[Post] = Post.findAll(By(Post.published, true))
      println ("the published ones: " + publishedPosts)

      println ("get post titles as string")
      val titlePost: List[String] = Post.findAllFields(Seq[SelectableField] (Post.title), 
          Like(Post.title, "This is%")
         ).map(_.title.is)
      println ("post titles as string: " + titlePost)
      println ("does not work either?")

  }

}