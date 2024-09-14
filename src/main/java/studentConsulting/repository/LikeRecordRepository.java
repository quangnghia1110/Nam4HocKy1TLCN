package studentConsulting.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import studentConsulting.model.entity.news.LikeKey;
import studentConsulting.model.entity.news.LikeRecord;

@Repository
public interface LikeRecordRepository extends JpaRepository<LikeRecord, LikeKey> {
	@Query("SELECT l FROM LikeRecord l WHERE l.likeKey.targetId = :postId AND l.likeKey.type = 'post'")
    List<LikeRecord> getLikeRecordsByPostId(Integer postId);

    @Query("SELECT l FROM LikeRecord l WHERE l.likeKey.targetId = :commentId AND l.likeKey.type = 'comment'")
    List<LikeRecord> getLikeRecordsByCommentId(Integer commentId);
    
    @Query("SELECT COUNT(lr) FROM LikeRecord lr WHERE lr.likeKey.targetId = :targetId AND lr.likeKey.type = :type")
    Integer countByLikeKeyTargetIdAndLikeKeyType(@Param("targetId") Integer targetId, @Param("type") String type);
}
