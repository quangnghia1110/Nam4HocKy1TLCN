package studentConsulting.controller.actor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.*;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.actor.CommentRepository;
import studentConsulting.repository.actor.PostRepository;
import studentConsulting.repository.actor.QuestionRepository;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.interfaces.actor.ILikeService;
import studentConsulting.service.interfaces.common.INotificationService;

import java.security.Principal;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class LikeController {

    private final ILikeService likeRecordService;
    private final CommentRepository commentRepository;
    private final PostRepository postRepository;
    private final QuestionRepository questionRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private INotificationService notificationService;

    @Autowired
    public LikeController(ILikeService likeRecordService, CommentRepository commentRepository,
                          PostRepository postRepository, QuestionRepository questionRepository) {
        this.likeRecordService = likeRecordService;
        this.commentRepository = commentRepository;
        this.postRepository = postRepository;
        this.questionRepository = questionRepository;
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/like/post")
    public ResponseEntity<DataResponse<String>> likePost(@RequestParam Integer postId, Principal principal) {
        if (!postRepository.existsById(postId)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(DataResponse.<String>builder().status("error").message("Bài viết không tồn tại.").build());
        }
        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity liker = userOpt.get();

        Integer userId = likeRecordService.getUserIdByEmail(email);
        likeRecordService.likePost(postId, userId);
        Optional<PostEntity> postOpt = postRepository.findById(postId);
        postOpt.ifPresent(post -> {
            UserInformationEntity postOwner = post.getUser();
            if (!postOwner.getId().equals(userId)) {
                notificationService.sendUserNotification(
                        liker.getId(),
                        postOwner.getId(),
                        NotificationContent.LIKE_POST.formatMessage(liker.getLastName() + " " + liker.getFirstName()),
                        NotificationType.USER
                );
            }
        });

        return ResponseEntity.ok(DataResponse.<String>builder().status("success")
                .message("Bạn đã thích bài viết này thành công.").build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/unlike/post")
    public ResponseEntity<DataResponse<String>> unlikePost(@RequestParam Integer postId, Principal principal) {
        if (!postRepository.existsById(postId)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(DataResponse.<String>builder().status("error").message("Bài viết không tồn tại.").build());
        }
        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        Integer userId = likeRecordService.getUserIdByEmail(email);
        likeRecordService.unlikePost(postId, userId);
        return ResponseEntity
                .ok(DataResponse.<String>builder().status("success").message("Bạn đã bỏ thích bài viết này.").build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/like/comment")
    public ResponseEntity<DataResponse<String>> likeComment(@RequestParam Integer commentId, Principal principal) {
        if (!commentRepository.existsById(commentId)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(DataResponse.<String>builder().status("error").message("Bình luận không tồn tại.").build());
        }
        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity liker = userOpt.get();

        Integer userId = likeRecordService.getUserIdByEmail(email);
        likeRecordService.likeComment(commentId, userId);
        Optional<CommentEntity> commentOpt = commentRepository.findById(commentId);
        commentOpt.ifPresent(comment -> {
            UserInformationEntity commentOwner = comment.getUserComment();
            if (!commentOwner.getId().equals(userId)) {
                notificationService.sendUserNotification(
                        liker.getId(),
                        commentOwner.getId(),
                        NotificationContent.LIKE_COMMENT.formatMessage(liker.getLastName() + " " + liker.getFirstName()),
                        NotificationType.USER
                );
            }
        });
        return ResponseEntity.ok(DataResponse.<String>builder().status("success")
                .message("Bạn đã thích bình luận này thành công.").build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/unlike/comment")
    public ResponseEntity<DataResponse<String>> unlikeComment(@RequestParam Integer commentId, Principal principal) {
        if (!commentRepository.existsById(commentId)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(DataResponse.<String>builder().status("error").message("Bình luận không tồn tại.").build());
        }
        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        Integer userId = likeRecordService.getUserIdByEmail(email);
        likeRecordService.unlikeComment(commentId, userId);
        return ResponseEntity
                .ok(DataResponse.<String>builder().status("success").message("Bạn đã bỏ thích bình luận này.").build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/like/question")
    public ResponseEntity<DataResponse<String>> likeQuestion(@RequestParam Integer questionId, Principal principal) {
        if (!questionRepository.existsById(questionId)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(DataResponse.<String>builder().status("error").message("Câu hỏi không tồn tại.").build());
        }

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        Integer userId = likeRecordService.getUserIdByEmail(email);
        likeRecordService.likeQuestion(questionId, userId);

        Optional<QuestionEntity> questionOpt = questionRepository.findById(questionId);
        questionOpt.ifPresent(question -> {
            UserInformationEntity questionOwner = question.getUser();
            if (!questionOwner.getId().equals(userId)) {
                notificationService.sendUserNotification(
                        userOpt.get().getId(),
                        questionOwner.getId(),
                        NotificationContent.LIKE_QUESTION.formatMessage(userOpt.get().getLastName() + " " + userOpt.get().getFirstName()),
                        NotificationType.USER
                );
            }
        });

        return ResponseEntity.ok(DataResponse.<String>builder().status("success")
                .message("Bạn đã thích câu hỏi này thành công.").build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/unlike/question")
    public ResponseEntity<DataResponse<String>> unlikeQuestion(@RequestParam Integer questionId, Principal principal) {
        if (!questionRepository.existsById(questionId)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(DataResponse.<String>builder().status("error").message("Câu hỏi không tồn tại.").build());
        }

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        Integer userId = likeRecordService.getUserIdByEmail(email);
        likeRecordService.unlikeQuestion(questionId, userId);

        return ResponseEntity
                .ok(DataResponse.<String>builder().status("success").message("Bạn đã bỏ thích câu hỏi này.").build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/like/post/check")
    public ResponseEntity<DataResponse<Boolean>> checkLikePost(
            @RequestParam Integer postId, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        boolean isLiked = likeRecordService.existsByUserAndPost(user, postId);

        return ResponseEntity.ok(DataResponse.<Boolean>builder()
                .status("success")
                .message("Kiểm tra like bài viết thành công.")
                .data(isLiked)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/like/comment/check")
    public ResponseEntity<DataResponse<Boolean>> checkLikeComment(
            @RequestParam Integer commentId, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        boolean isLiked = likeRecordService.existsByUserAndComment(user, commentId);

        return ResponseEntity.ok(DataResponse.<Boolean>builder()
                .status("success")
                .message("Kiểm tra like bình luận thành công.")
                .data(isLiked)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/like/question/check")
    public ResponseEntity<DataResponse<Boolean>> checkLikeQuestion(
            @RequestParam Integer questionId, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        boolean isLiked = likeRecordService.existsByUserAndQuestion(user, questionId);

        return ResponseEntity.ok(DataResponse.<Boolean>builder()
                .status("success")
                .message("Kiểm tra like câu hỏi thành công.")
                .data(isLiked)
                .build());
    }



}
