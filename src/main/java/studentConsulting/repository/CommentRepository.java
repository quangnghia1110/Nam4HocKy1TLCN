package studentConsulting.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.news.Comment;

import java.util.List;

@Repository
public interface CommentRepository extends JpaRepository<Comment, Long> {
    
    List<Comment> getCommentByPostId(Long idPost);
    
    @Query("SELECT c FROM Comment c WHERE c.parentComment IS NULL")
    List<Comment> getCommentRoot();  // Sửa lỗi truy vấn để lấy các comment không có cha
    
    @Query("SELECT c FROM Comment c WHERE c.parentComment.idComment = :id_father")
    List<Comment> getCommentByFather(@Param("id_father") Long idFather);

    @Query("SELECT c FROM Comment c WHERE c.parentComment.idComment = :parentCommentId")
    List<Comment> getCommentByParentComment(@Param("parentCommentId") Long parentCommentId);

    @Query("SELECT c FROM Comment c WHERE c.post.id = :postId AND c.parentComment IS NULL")
    List<Comment> getRootCommentByPostId(@Param("postId") Long postId);  // Lấy comment gốc theo postId
}

