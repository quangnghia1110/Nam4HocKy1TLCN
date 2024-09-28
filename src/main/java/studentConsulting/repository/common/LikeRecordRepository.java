package studentConsulting.repository.common;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import studentConsulting.model.entity.content.LikeKeyEntity;
import studentConsulting.model.entity.content.LikeRecordEntity;

import java.util.List;

@Repository
public interface LikeRecordRepository extends JpaRepository<LikeRecordEntity, LikeKeyEntity> {
    @Query("SELECT l FROM LikeRecordEntity l WHERE l.likeKey.targetId = :postId AND l.likeKey.type = 'post'")
    List<LikeRecordEntity> getLikeRecordsByPostId(Integer postId);

    @Query("SELECT l FROM LikeRecordEntity l WHERE l.likeKey.targetId = :commentId AND l.likeKey.type = 'comment'")
    List<LikeRecordEntity> getLikeRecordsByCommentId(Integer commentId);

    @Query("SELECT COUNT(lr) FROM LikeRecordEntity lr WHERE lr.likeKey.targetId = :targetId AND lr.likeKey.type = :type")
    Integer countByLikeKeyTargetIdAndLikeKeyType(@Param("targetId") Integer targetId, @Param("type") String type);
}
