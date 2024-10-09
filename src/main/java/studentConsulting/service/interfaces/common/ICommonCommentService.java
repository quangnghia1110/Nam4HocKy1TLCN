package studentConsulting.service.interfaces.common;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.content.CommentDTO;
import studentConsulting.model.payload.response.DataResponse;

import java.time.LocalDate;
import java.util.Hashtable;
import java.util.List;
import java.util.Optional;

public interface ICommonCommentService {

    DataResponse<List<Hashtable<String, Object>>> getAllComments(Integer postId);

    List<Hashtable<String, Object>> getCommentChild(Integer idCommentFather);

    Hashtable<String, Object> getCommentById(Integer id_comment);

    CommentDTO createComment(Integer idPost, String text, String username);

    CommentDTO replyComment(Integer commentFatherId, String text, String username);

    Hashtable<String, Object> updateComment(Integer idComment, String text, String email);

    Hashtable<String, Object> adminUpdateComment(Integer idComment, String text);

    void deleteComment(Integer idComment, String email);

    void adminDeleteComment(Integer idComment);

    Page<CommentDTO> getCommentsByPostWithPagingAndFilters(Optional<Integer> postId, Optional<LocalDate> startDate, Optional<LocalDate> endDate, Pageable pageable);

}
