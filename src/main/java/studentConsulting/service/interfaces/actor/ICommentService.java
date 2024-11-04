package studentConsulting.service.interfaces.actor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.actor.CommentDTO;
import studentConsulting.model.payload.response.DataResponse;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface ICommentService {

    DataResponse<List<CommentDTO>> getAllComments(Integer postId);

    List<CommentDTO> getCommentChild(Integer idCommentFather);

    CommentDTO getCommentById(Integer id_comment);

    CommentDTO createComment(Integer idPost, String text, String username);

    CommentDTO replyComment(Integer commentFatherId, String text, String username);

    CommentDTO updateComment(Integer idComment, String text, String email);

    CommentDTO adminUpdateComment(Integer idComment, String text);

    void deleteComment(Integer idComment, String email);

    void adminDeleteComment(Integer idComment);

    Page<CommentDTO> getCommentsByPostWithPagingAndFilters(Optional<Integer> postId, Optional<LocalDate> startDate, Optional<LocalDate> endDate, Pageable pageable);

}
