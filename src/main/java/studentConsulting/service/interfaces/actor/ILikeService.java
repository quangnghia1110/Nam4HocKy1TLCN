package studentConsulting.service.interfaces.actor;

import studentConsulting.model.entity.LikeRecordEntity;
import studentConsulting.model.entity.UserInformationEntity;

import java.util.List;

public interface ILikeService {

    List<LikeRecordEntity> getLikeRecordByPostId(Integer postId);

    List<LikeRecordEntity> getLikeRecordByCommentId(Integer commentId);

    void likePost(Integer postId, Integer userId);

    void unlikePost(Integer postId, Integer userId);

    void likeComment(Integer commentId, Integer userId);

    void unlikeComment(Integer commentId, Integer userId);

    Integer getUserIdByEmail(String email);

    Integer countLikesByPostId(Integer postId);

    Integer countLikesByCommentId(Integer commentId);

    void likeQuestion(Integer questionId, Integer userId);

    void unlikeQuestion(Integer questionId, Integer userId);

    Integer countLikesByQuestionId(Integer questionId);

    boolean existsByUserAndPost(UserInformationEntity user, Integer postId);
    boolean existsByUserAndComment(UserInformationEntity user, Integer commentId);
    boolean existsByUserAndQuestion(UserInformationEntity user, Integer questionId);



    }
