package studentConsulting.repository.content;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.content.CommentEntity;
import studentConsulting.model.entity.content.PostEntity;

import javax.transaction.Transactional;
import java.util.List;

@Repository
public interface CommentRepository extends JpaRepository<CommentEntity, Integer> {

    List<CommentEntity> getCommentByPostId(Integer idPost);

    @Query("SELECT c FROM CommentEntity c WHERE c.parentComment IS NULL")
    List<CommentEntity> getCommentRoot();  // Sửa lỗi truy vấn để lấy các comment không có cha

    @Query("SELECT c FROM CommentEntity c WHERE c.parentComment.idComment = :id_father")
    List<CommentEntity> getCommentByFather(@Param("id_father") Integer idFather);

    @Query("SELECT c FROM CommentEntity c WHERE c.parentComment.idComment = :parentCommentId")
    List<CommentEntity> getCommentByParentComment(@Param("parentCommentId") Integer parentCommentId);

    @Query("SELECT c FROM CommentEntity c WHERE c.post.id = :postId AND c.parentComment IS NULL")
    List<CommentEntity> getRootCommentByPostId(@Param("postId") Integer postId);  // Lấy comment gốc theo postId

    @Modifying
    @Transactional
    void deleteByPost(PostEntity post);
}

