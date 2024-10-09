package studentConsulting.service.implement.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.content.CommentEntity;
import studentConsulting.model.entity.content.PostEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.content.CommentDTO;
import studentConsulting.model.payload.dto.user.UserDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.content.CommentRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.common.ICommonCommentService;

import javax.transaction.Transactional;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Optional;

@Service
@Transactional
public class CommonCommentServiceImpl implements ICommonCommentService {

    @Autowired
    private CommentRepository commentRepository;

    @Autowired
    private UserRepository userRepository;

    @Override
    public DataResponse<List<Hashtable<String, Object>>> getAllComments(Integer postId) {
        try {
            List<Hashtable<String, Object>> result = new ArrayList<>();
            List<CommentEntity> comments = commentRepository.getRootCommentByPostId(postId);  // Lấy các comment gốc

            for (CommentEntity comment : comments) {
                Hashtable<String, Object> commentData = new Hashtable<>();
                commentData.put("id_comment", comment.getIdComment());
                commentData.put("id_post", comment.getPost().getId());
                commentData.put("id_user_comment", comment.getUserComment().getId());
                commentData.put("comment", comment.getComment());
                commentData.put("create_date", comment.getCreateDate());
                commentData.put("child_comments", getCommentChild(comment.getIdComment()));  // Lấy các comment con

                result.add(commentData);
            }

            return DataResponse.<List<Hashtable<String, Object>>>builder()
                    .status("success")
                    .message("Danh sách bình luận")
                    .data(result)
                    .build();
        } catch (Exception e) {
            throw new ErrorException("Đã xảy ra lỗi khi lấy danh sách bình luận.");
        }
    }


    @Override
    public List<Hashtable<String, Object>> getCommentChild(Integer idCommentFather) {
        List<Hashtable<String, Object>> result = new ArrayList<>();
        List<CommentEntity> comments = commentRepository.getCommentByParentComment(idCommentFather);

        for (CommentEntity comment : comments) {
            Hashtable<String, Object> commentData = new Hashtable<>();
            commentData.put("id_comment", comment.getIdComment());
            commentData.put("id_post", comment.getPost().getId());
            commentData.put("id_user_comment", comment.getUserComment().getId());
            commentData.put("comment", comment.getComment());
            commentData.put("create_date", comment.getCreateDate());
            commentData.put("child_comments", getCommentChild(comment.getIdComment()));  // Đệ quy lấy các comment con

            result.add(commentData);
        }

        return result;
    }


    @Override
    public CommentDTO createComment(Integer idPost, String text, String email) {

        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        CommentEntity comment = new CommentEntity();
        comment.setPost(new PostEntity(idPost));
        comment.setComment(text);
        comment.setUserComment(user);
        comment.setCreateDate(LocalDate.now());

        commentRepository.save(comment);

        return convertToCommentDTO(comment);
    }

    @Override
    public CommentDTO replyComment(Integer commentFatherId, String text, String email) {

        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        CommentEntity parentComment = commentRepository.findById(commentFatherId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy bình luận cha."));

        CommentEntity comment = new CommentEntity();
        comment.setParentComment(parentComment);
        comment.setPost(parentComment.getPost());
        comment.setComment(text);
        comment.setUserComment(user);
        comment.setCreateDate(LocalDate.now());

        commentRepository.save(comment);

        return convertToCommentDTO(comment);
    }

    public CommentDTO convertToCommentDTO(CommentEntity comment) {
        UserDTO userDTO = new UserDTO();
        userDTO.setId(comment.getUserComment().getId());
        userDTO.setFirstName(comment.getUserComment().getFirstName());
        userDTO.setLastName(comment.getUserComment().getLastName());

        CommentDTO dto = new CommentDTO();
        dto.setText(comment.getComment());
        dto.setUser(userDTO);
        dto.setCreate_date(comment.getCreateDate());

        return dto;
    }

    @Override
    public Hashtable<String, Object> updateComment(Integer idComment, String text, String email) {
        Optional<CommentEntity> commentOpt = commentRepository.findById(idComment);
        if (commentOpt.isEmpty()) {
            throw new ErrorException("Không tìm thấy bình luận.");
        }

        CommentEntity comment = commentOpt.get();

        if (!comment.getUserComment().getAccount().getEmail().equals(email)) {
            throw new ErrorException("Bạn không có quyền cập nhật bình luận này.");
        }

        comment.setComment(text);
        comment.setCreateDate(LocalDate.now());
        commentRepository.save(comment);

        return getCommentById(idComment);
    }

    @Override
    public Hashtable<String, Object> adminUpdateComment(Integer idComment, String text) {
        Optional<CommentEntity> commentOpt = commentRepository.findById(idComment);
        if (commentOpt.isEmpty()) {
            throw new ErrorException("Không tìm thấy bình luận.");
        }

        CommentEntity comment = commentOpt.get();
        comment.setComment(text);
        comment.setCreateDate(LocalDate.now());
        commentRepository.save(comment);

        return getCommentById(idComment);
    }

    @Override
    public void deleteComment(Integer idComment, String email) {
        Optional<CommentEntity> commentOpt = commentRepository.findById(idComment);
        if (commentOpt.isEmpty()) {
            throw new ErrorException("Không tìm thấy bình luận.");
        }

        CommentEntity comment = commentOpt.get();

        if (!comment.getUserComment().getAccount().getEmail().equals(email)) {
            throw new ErrorException("Bạn không có quyền xóa bình luận này.");
        }

        List<CommentEntity> children = commentRepository.getCommentByParentComment(idComment);
        children.forEach(cmt -> deleteComment(cmt.getIdComment(), email));

        commentRepository.deleteById(idComment);
    }

    @Override
    public void adminDeleteComment(Integer idComment) {
        commentRepository.findById(idComment)
                .orElseThrow(() -> new ErrorException("Không tìm thấy bình luận."));
        List<CommentEntity> children = commentRepository.getCommentByParentComment(idComment);

        children.forEach(cmt -> adminDeleteComment(cmt.getIdComment()));
        commentRepository.deleteById(idComment);
    }


    @Override
    public Hashtable<String, Object> getCommentById(Integer idComment) {
        CommentEntity comment = commentRepository.findById(idComment)
                .orElseThrow(() -> new ErrorException("Không tìm thấy bình luận."));
        Hashtable<String, Object> commentData = new Hashtable<>();
        commentData.put("id_comment", comment.getIdComment());
        commentData.put("id_post", comment.getPost().getId());
        commentData.put("id_user_comment", comment.getUserComment().getId());
        commentData.put("comment", comment.getComment());
        commentData.put("create_date", comment.getCreateDate());

        return commentData;
    }
}
