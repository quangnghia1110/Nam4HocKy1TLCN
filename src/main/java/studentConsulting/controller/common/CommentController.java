package studentConsulting.controller.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.CommentDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.interfaces.common.ICommonCommentService;

import java.security.Principal;
import java.util.Hashtable;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class CommentController {

    @Autowired
    private ICommonCommentService commentService;

    @Autowired
    private UserRepository userRepository;

    @PostMapping("/comment/create")
    public ResponseEntity<DataResponse<CommentDTO>> createComment(@RequestParam Integer postId,
                                                                  @RequestParam String text, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        CommentDTO createdComment = commentService.createComment(postId, text, email);

        return ResponseEntity.ok(DataResponse.<CommentDTO>builder().status("success")
                .message("Bình luận đã được tạo thành công").data(createdComment).build());
    }

    @PostMapping("/comment/reply")
    public ResponseEntity<DataResponse<CommentDTO>> replyComment(@RequestParam Integer commentFatherId,
                                                                 @RequestParam String text, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        CommentDTO replyComment = commentService.replyComment(commentFatherId, text, email);

        return ResponseEntity.ok(DataResponse.<CommentDTO>builder().status("success")
                .message("Bình luận đã được trả lời thành công").data(replyComment).build());
    }

    @PutMapping("/comment/update")
    public ResponseEntity<DataResponse<Hashtable<String, Object>>> updateComment(@RequestParam Integer commentId,
                                                                                 @RequestParam String text) {

        Hashtable<String, Object> updatedComment = commentService.updateComment(commentId, text);

        return ResponseEntity.ok(DataResponse.<Hashtable<String, Object>>builder().status("success")
                .message("Bình luận đã được cập nhật thành công").data(updatedComment).build());
    }

    @GetMapping("/comment/get-comment-by-post")
    public ResponseEntity<DataResponse<List<Hashtable<String, Object>>>> getCommentsByPost(
            @RequestParam Integer postId) {
        DataResponse<List<Hashtable<String, Object>>> response = commentService.getAllComments(postId);

        if (response.getData().isEmpty()) {
            throw new ErrorException("Không có bình luận nào cho bài viết này.");
        }

        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @DeleteMapping("/comment/delete")
    public ResponseEntity<DataResponse<Void>> deleteComment(@RequestParam Integer commentId) {
        commentService.deleteComment(commentId);
        return ResponseEntity
                .ok(DataResponse.<Void>builder().status("success").message("Bình luận đã được xóa").build());
    }
}
