package studentConsulting.service.implement.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import studentConsulting.constant.enums.LikeType;
import studentConsulting.model.entity.content.LikeKeyEntity;
import studentConsulting.model.entity.content.LikeRecordEntity;
import studentConsulting.repository.content.LikeRecordRepository;
import studentConsulting.service.interfaces.common.ICommonLikeRecordService;
import studentConsulting.service.interfaces.common.ICommonUserService;

import javax.transaction.Transactional;
import java.util.List;

@Service
@Transactional
public class CommonLikeRecordServiceImpl implements ICommonLikeRecordService {

    private final LikeRecordRepository likeRecordRepository;
    private final ICommonUserService userService;

    @Autowired
    public CommonLikeRecordServiceImpl(LikeRecordRepository likeRecordRepository, @Lazy ICommonUserService userService) {
        this.likeRecordRepository = likeRecordRepository;
        this.userService = userService;
    }

    @Override
    public List<LikeRecordEntity> getLikeRecordByPostId(Integer postId) {
        return likeRecordRepository.getLikeRecordsByPostId(postId);
    }

    @Override
    public List<LikeRecordEntity> getLikeRecordByCommentId(Integer commentId) {
        return likeRecordRepository.getLikeRecordsByCommentId(commentId);
    }

    @Override
    public void likePost(Integer postId, Integer userId) {
        LikeRecordEntity likeRecord = new LikeRecordEntity(new LikeKeyEntity(postId, userId, LikeType.POST.toString()));
        likeRecordRepository.save(likeRecord);
    }

    @Override
    public void unlikePost(Integer postId, Integer userId) {
        LikeRecordEntity likeRecord = new LikeRecordEntity(new LikeKeyEntity(postId, userId, LikeType.POST.toString()));
        likeRecordRepository.delete(likeRecord);
    }

    @Override
    public void likeComment(Integer commentId, Integer userId) {
        LikeRecordEntity likeRecord = new LikeRecordEntity(new LikeKeyEntity(commentId, userId, LikeType.COMMENT.toString()));
        likeRecordRepository.save(likeRecord);
    }

    @Override
    public void unlikeComment(Integer commentId, Integer userId) {
        LikeRecordEntity likeRecord = new LikeRecordEntity(new LikeKeyEntity(commentId, userId, LikeType.COMMENT.toString()));
        likeRecordRepository.delete(likeRecord);
    }

    @Override
    public Integer getUserIdByEmail(String email) {
        return userService.getUserIdByEmail(email);
    }

    @Override
    public Integer countLikesByPostId(Integer postId) {
        return likeRecordRepository.countByLikeKeyTargetIdAndLikeKeyType(postId, "post");
    }

    @Override
    public Integer countLikesByCommentId(Integer commentId) {
        return likeRecordRepository.countByLikeKeyTargetIdAndLikeKeyType(commentId, "comment");
    }
}
