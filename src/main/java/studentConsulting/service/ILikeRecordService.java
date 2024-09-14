package studentConsulting.service;

import java.util.List;

import studentConsulting.model.entity.news.LikeRecord;
import studentConsulting.model.payload.dto.UserDTO;

public interface ILikeRecordService {

	List<LikeRecord> getLikeRecordByPostId(Integer postId);

	List<LikeRecord> getLikeRecordByCommentId(Integer commentId);

	void likePost(Integer postId, Integer userId);

	void unlikePost(Integer postId, Integer userId);

	void likeComment(Integer commentId, Integer userId);

	void unlikeComment(Integer commentId, Integer userId);

	Integer getUserIdByUsername(String username);

	Integer countLikesByPostId(Integer postId);

	Integer countLikesByCommentId(Integer commentId);
}
