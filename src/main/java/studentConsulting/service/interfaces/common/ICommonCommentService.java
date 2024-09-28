package studentConsulting.service.interfaces.common;

import studentConsulting.model.payload.dto.CommentDTO;
import studentConsulting.model.payload.response.DataResponse;

import java.util.Hashtable;
import java.util.List;

public interface ICommonCommentService {

    DataResponse<List<Hashtable<String, Object>>> getAllComments(Integer postId);


    List<Hashtable<String, Object>> getCommentChild(Integer idCommentFather);


    Hashtable<String, Object> getCommentById(Integer id_comment);

    CommentDTO createComment(Integer idPost, String text, String username);

    CommentDTO replyComment(Integer commentFatherId, String text, String username);

    Hashtable<String, Object> updateComment(Integer id_comment, String text);

    void deleteComment(Integer commentId);
}
