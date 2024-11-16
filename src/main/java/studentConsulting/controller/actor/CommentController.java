package studentConsulting.controller.actor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.CommentEntity;
import studentConsulting.model.entity.PostEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.CommentDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.actor.CommentRepository;
import studentConsulting.repository.actor.PostRepository;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.interfaces.actor.ICommentService;
import studentConsulting.service.interfaces.common.IExcelService;
import studentConsulting.service.interfaces.common.INotificationService;
import studentConsulting.service.interfaces.common.IPdfService;

import java.security.Principal;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class CommentController {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommentService commentService;

    @Autowired
    private IExcelService excelService;

    @Autowired
    private IPdfService pdfService;

    @Autowired
    private INotificationService notificationService;

    @Autowired
    private PostRepository postRepository;

    @Autowired
    private CommentRepository commentRepository;

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
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
        Optional<PostEntity> postOpt = postRepository.findById(postId);
        if (postOpt.isPresent()) {
            UserInformationEntity postOwner = postOpt.get().getUser();

            notificationService.sendUserNotification(
                    userOpt.get().getId(),
                    postOwner.getId(),
                    NotificationContent.NEW_COMMENT.formatMessage(userOpt.get().getLastName() + " " + userOpt.get().getFirstName()),
                    NotificationType.USER
            );
        }
        return ResponseEntity.ok(DataResponse.<CommentDTO>builder().status("success")
                .message("Bình luận đã được tạo thành công").data(createdComment).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
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

        Optional<CommentEntity> commentOpt = commentRepository.findById(commentFatherId);
        if (commentOpt.isPresent()) {
            CommentEntity originalComment = commentOpt.get();
            UserInformationEntity commentOwner = originalComment.getUserComment();
            PostEntity post = originalComment.getPost();
            UserInformationEntity postOwner = post.getUser();

            notificationService.sendUserNotification(
                    userOpt.get().getId(),
                    postOwner.getId(),
                    NotificationContent.NEW_REPLY_POST.formatMessage(userOpt.get().getLastName() + " " + userOpt.get().getFirstName()),
                    NotificationType.USER
            );

            if (!commentOwner.getId().equals(postOwner.getId())) {
                notificationService.sendUserNotification(
                        userOpt.get().getId(),
                        commentOwner.getId(),
                        NotificationContent.NEW_REPLY_COMMENT.formatMessage(userOpt.get().getLastName() + " " + userOpt.get().getFirstName()),
                        NotificationType.USER
                );
            }
        }

        return ResponseEntity.ok(DataResponse.<CommentDTO>builder()
                .status("success")
                .message("Bình luận đã được trả lời thành công")
                .data(replyComment)
                .build());
    }


    private boolean isAdminFromDB(String email) {
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (userOpt.isPresent()) {
            UserInformationEntity user = userOpt.get();
            return user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        }
        return false;
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/comment/update")
    public ResponseEntity<DataResponse<CommentDTO>> updateComment(@RequestParam Integer commentId,
                                                                  @RequestParam String text,
                                                                  Principal principal) {

        String email = principal.getName();
        boolean isAdmin = isAdminFromDB(email);

        CommentDTO updatedComment;
        if (isAdmin) {
            updatedComment = commentService.adminUpdateComment(commentId, text);
        } else {
            updatedComment = commentService.updateComment(commentId, text, email);
        }

        return ResponseEntity.ok(DataResponse.<CommentDTO>builder()
                .status("success")
                .message("Bình luận đã được cập nhật thành công")
                .data(updatedComment)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/comment/delete")
    public ResponseEntity<DataResponse<Void>> deleteComment(@RequestParam Integer commentId, Principal principal) {

        String email = principal.getName();
        boolean isAdmin = isAdminFromDB(email);

        if (isAdmin) {
            commentService.adminDeleteComment(commentId);
        } else {
            commentService.deleteComment(commentId, email);
        }

        return ResponseEntity
                .ok(DataResponse.<Void>builder().status("success").message("Bình luận đã được xóa").build());
    }
}
