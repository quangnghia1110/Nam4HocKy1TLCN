package studentConsulting.service;

import studentConsulting.model.entity.news.Comment;
import studentConsulting.model.payload.dto.CommentDTO;
import studentConsulting.model.payload.response.DataResponse;

import java.util.Hashtable;
import java.util.List;

public interface ICommentService {

    public DataResponse<List<Hashtable<String, Object>>> getAllComments(Long postId);

    
	public List<Hashtable<String, Object>> getCommentChild(Long idCommentFather);
    

    Hashtable<String, Object> getCommentById(Long id_comment);

    CommentDTO createComment(Long idPost, String text, String username);
    
    CommentDTO replyComment(Long commentFatherId, String text, String username);
    
    Hashtable<String, Object> updateComment(Long id_comment, String text);

    void deleteComment(Long commentId);
}
