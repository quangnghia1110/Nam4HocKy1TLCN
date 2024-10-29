package studentConsulting.controller.actor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.content.LikeRecordEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.content.PostRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.repository.content.CommentRepository;
import studentConsulting.service.interfaces.actor.ILikeService;

import java.security.Principal;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class LikeController {

    private final ILikeService likeRecordService;
    private final CommentRepository commentRepository;
    private final PostRepository postRepository;
    @Autowired
    private UserRepository userRepository;

    @Autowired
    public LikeController(ILikeService likeRecordService, CommentRepository commentRepository,
                          PostRepository postRepository) {
        this.likeRecordService = likeRecordService;
        this.commentRepository = commentRepository;
        this.postRepository = postRepository;
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
        Integer userId = likeRecordService.getUserIdByEmail(email);
        likeRecordService.likePost(postId, userId);
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
        Integer userId = likeRecordService.getUserIdByEmail(email);
        likeRecordService.likeComment(commentId, userId);
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


    @GetMapping("/like-records/post")
    public ResponseEntity<DataResponse<List<LikeRecordEntity>>> getLikeRecordByPostId(@RequestParam Integer postId) {
        List<LikeRecordEntity> likeRecords = likeRecordService.getLikeRecordByPostId(postId);
        if (likeRecords == null || likeRecords.isEmpty()) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(DataResponse.<List<LikeRecordEntity>>builder()
                    .status("error").message("Không tìm thấy danh sách thích cho bài viết này.").build());
        }
        return ResponseEntity.ok(DataResponse.<List<LikeRecordEntity>>builder().status("success")
                .message("Danh sách thích của bài viết đã được lấy thành công.").data(likeRecords).build());
    }

    @GetMapping("/like-records/comment")
    public ResponseEntity<DataResponse<List<LikeRecordEntity>>> getLikeRecordByCommentId(@RequestParam Integer commentId) {
        List<LikeRecordEntity> likeRecords = likeRecordService.getLikeRecordByCommentId(commentId);
        if (likeRecords == null || likeRecords.isEmpty()) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(DataResponse.<List<LikeRecordEntity>>builder()
                    .status("error").message("Không tìm thấy danh sách thích cho bình luận này.").build());
        }
        return ResponseEntity.ok(DataResponse.<List<LikeRecordEntity>>builder().status("success")
                .message("Danh sách thích của bình luận đã được lấy thành công.").data(likeRecords).build());
    }

    @GetMapping("/like-count/post")
    public ResponseEntity<DataResponse<Integer>> countLikesByPostId(@RequestParam Integer postId) {
        if (!postRepository.existsById(postId)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(DataResponse.<Integer>builder().status("error").message("Bài viết không tồn tại.").build());
        }
        Integer count = likeRecordService.countLikesByPostId(postId);
        return ResponseEntity.ok(DataResponse.<Integer>builder().status("success")
                .message("Số lượt thích của bài viết đã được lấy thành công.").data(count).build());
    }

    @GetMapping("/like-count/comment")
    public ResponseEntity<DataResponse<Integer>> countLikesByCommentId(@RequestParam Integer commentId) {
        if (!commentRepository.existsById(commentId)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(DataResponse.<Integer>builder().status("error").message("Bình luận không tồn tại.").build());
        }
        Integer count = likeRecordService.countLikesByCommentId(commentId);
        return ResponseEntity.ok(DataResponse.<Integer>builder().status("success")
                .message("Số lượt thích của bình luận đã được lấy thành công.").data(count).build());
    }

}
