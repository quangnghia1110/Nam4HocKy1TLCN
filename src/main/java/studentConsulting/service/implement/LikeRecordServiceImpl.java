package studentConsulting.service.implement;

import java.util.List;
import java.util.stream.Collectors;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import studentConsulting.constant.enums.LikeType;
import studentConsulting.model.entity.news.LikeKey;
import studentConsulting.model.entity.news.LikeRecord;
import studentConsulting.model.payload.dto.UserDTO;
import studentConsulting.repository.LikeRecordRepository;
import studentConsulting.service.ILikeRecordService;
import studentConsulting.service.IUserService;

@Service
@Transactional
public class LikeRecordServiceImpl implements ILikeRecordService {

    private final LikeRecordRepository likeRecordRepository;
    private final IUserService userService; 
    @Autowired
    public LikeRecordServiceImpl(LikeRecordRepository likeRecordRepository, @Lazy IUserService userService) {
        this.likeRecordRepository = likeRecordRepository;
        this.userService = userService;
    }

    @Override
    public List<LikeRecord> getLikeRecordByPostId(Integer postId) {
        return likeRecordRepository.getLikeRecordsByPostId(postId);
    }

    @Override
    public List<LikeRecord> getLikeRecordByCommentId(Integer commentId) {
        return likeRecordRepository.getLikeRecordsByCommentId(commentId);
    }

    @Override
    public void likePost(Integer postId, Integer userId) {
        LikeRecord likeRecord = new LikeRecord(new LikeKey(postId, userId, LikeType.POST.toString()));
        likeRecordRepository.save(likeRecord);
    }

    @Override
    public void unlikePost(Integer postId, Integer userId) {
        LikeRecord likeRecord = new LikeRecord(new LikeKey(postId, userId, LikeType.POST.toString()));
        likeRecordRepository.delete(likeRecord);
    }

    @Override
    public void likeComment(Integer commentId, Integer userId) {
        LikeRecord likeRecord = new LikeRecord(new LikeKey(commentId, userId, LikeType.COMMENT.toString()));
        likeRecordRepository.save(likeRecord);
    }

    @Override
    public void unlikeComment(Integer commentId, Integer userId) {
        LikeRecord likeRecord = new LikeRecord(new LikeKey(commentId, userId, LikeType.COMMENT.toString()));
        likeRecordRepository.delete(likeRecord);
    }

    @Override
    public Integer getUserIdByUsername(String username) {
        return userService.getUserIdByUsername(username);  
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
