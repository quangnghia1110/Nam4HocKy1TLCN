package studentConsulting.service.implement;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.news.Comment;
import studentConsulting.model.entity.news.PostEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.CommentDTO;
import studentConsulting.model.payload.dto.UserDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.repository.CommentRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.ICommentService;

import javax.transaction.Transactional;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
@Transactional
public class CommentServiceImpl implements ICommentService {

    @Autowired
    private CommentRepository commentRepository;

    @Autowired
    private UserRepository userRepository;

    @Override
    public DataResponse<List<Hashtable<String, Object>>> getAllComments(Integer postId) {
        try {
            List<Hashtable<String, Object>> result = new ArrayList<>();
            List<Comment> comments = commentRepository.getRootCommentByPostId(postId);  // Lấy các comment gốc

            for (Comment comment : comments) {
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
        List<Comment> comments = commentRepository.getCommentByParentComment(idCommentFather);

        for (Comment comment : comments) {
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
    public CommentDTO createComment(Integer idPost, String text, String username) {
        Optional<UserInformationEntity> userOptional = userRepository.findByAccountUsername(username);
        if (userOptional.isEmpty()) {
            throw new ErrorException("Không tìm thấy người dùng.");
        }

        UserInformationEntity user = userOptional.get();
        Comment comment = new Comment();
        comment.setPost(new PostEntity(idPost));
        comment.setComment(text);
        comment.setUserComment(user);
        comment.setCreateDate(LocalDate.now());
        
        commentRepository.save(comment);
        
        return convertToCommentDTO(comment); 
    }

    @Override
    public CommentDTO replyComment(Integer commentFatherId, String text, String username) {
        Optional<UserInformationEntity> userOptional = userRepository.findByAccountUsername(username);
        if (userOptional.isEmpty()) {
            throw new ErrorException("Không tìm thấy người dùng.");
        }

        UserInformationEntity user = userOptional.get();
        Comment parentComment = commentRepository.findById(commentFatherId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy bình luận cha."));

        Comment comment = new Comment();
        comment.setParentComment(parentComment); 
        comment.setPost(parentComment.getPost()); 
        comment.setComment(text);
        comment.setUserComment(user);
        comment.setCreateDate(LocalDate.now());

        commentRepository.save(comment);
        
        return convertToCommentDTO(comment);
    }

    public CommentDTO convertToCommentDTO(Comment comment) {
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
    public Hashtable<String, Object> updateComment(Integer idComment, String text) {
        Optional<Comment> commentOpt = commentRepository.findById(idComment);
        if (commentOpt.isEmpty()) {
            throw new ErrorException("Không tìm thấy bình luận.");
        }

        Comment comment = commentOpt.get();
        comment.setComment(text);
        comment.setCreateDate(LocalDate.now());
        commentRepository.save(comment);

        return getCommentById(idComment);
    }

    @Override
    public void deleteComment(Integer idComment) {
        Comment comment = commentRepository.findById(idComment)
                .orElseThrow(() -> new ErrorException("Không tìm thấy bình luận."));
        List<Comment> children = commentRepository.getCommentByParentComment(idComment);

        children.forEach(cmt -> deleteComment(cmt.getIdComment()));
        commentRepository.deleteById(idComment);
    }
    
    

    public Hashtable<String, Object> getCommentById(Integer idComment) {
        Comment comment = commentRepository.findById(idComment)
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
