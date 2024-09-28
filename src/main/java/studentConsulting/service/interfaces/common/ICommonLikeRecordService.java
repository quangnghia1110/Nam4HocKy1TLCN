package studentConsulting.service.interfaces.common;

import studentConsulting.model.entity.news.LikeRecord;

import java.util.List;

public interface ICommonLikeRecordService {

    List<LikeRecord> getLikeRecordByPostId(Integer postId);

    List<LikeRecord> getLikeRecordByCommentId(Integer commentId);

    void likePost(Integer postId, Integer userId);

    void unlikePost(Integer postId, Integer userId);

    void likeComment(Integer commentId, Integer userId);

    void unlikeComment(Integer commentId, Integer userId);

    Integer getUserIdByEmail(String email);

    Integer countLikesByPostId(Integer postId);

    Integer countLikesByCommentId(Integer commentId);
}
