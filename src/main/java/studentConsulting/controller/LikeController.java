package studentConsulting.controller;

import java.security.Principal;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.entity.news.LikeRecord;
import studentConsulting.model.payload.dto.UserDTO;
import studentConsulting.service.ICommentService;
import studentConsulting.service.ILikeRecordService;
import studentConsulting.service.IPostService;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.CommentRepository;
import studentConsulting.repository.PostRepository;

@RestController
@RequestMapping("${base.url}")
public class LikeController {

	private final ILikeRecordService likeRecordService;
    private final CommentRepository commentRepository;   
    private final PostRepository postRepository;
    
    @Autowired
    public LikeController(ILikeRecordService likeRecordService, CommentRepository commentRepository, PostRepository postRepository) {
        this.likeRecordService = likeRecordService;
        this.commentRepository = commentRepository;
        this.postRepository = postRepository;
    }

    @PostMapping("/like/post")
    public ResponseEntity<DataResponse<String>> likePost(@RequestParam Integer postId, Principal principal) {
        if (!postRepository.existsById(postId)) { 
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(
                DataResponse.<String>builder()
                    .status("error")
                    .message("Bài viết không tồn tại.")
                    .build()
            );
        }
        String username = principal.getName();
        Integer userId = likeRecordService.getUserIdByUsername(username);
        likeRecordService.likePost(postId, userId);
        return ResponseEntity.ok(DataResponse.<String>builder()
                .status("success")
                .message("Bạn đã thích bài viết này thành công.")
                .build());
    }

    @DeleteMapping("/unlike/post")
    public ResponseEntity<DataResponse<String>> unlikePost(@RequestParam Integer postId, Principal principal) {
        if (!postRepository.existsById(postId)) { 
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(
                DataResponse.<String>builder()
                    .status("error")
                    .message("Bài viết không tồn tại.")
                    .build()
            );
        }
        String username = principal.getName();
        Integer userId = likeRecordService.getUserIdByUsername(username);
        likeRecordService.unlikePost(postId, userId);
        return ResponseEntity.ok(DataResponse.<String>builder()
                .status("success")
                .message("Bạn đã bỏ thích bài viết này.")
                .build());
    }

    @PostMapping("/like/comment")
    public ResponseEntity<DataResponse<String>> likeComment(@RequestParam Integer commentId, Principal principal) {
        if (!commentRepository.existsById(commentId)) {  
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(
                DataResponse.<String>builder()
                    .status("error")
                    .message("Bình luận không tồn tại.")
                    .build()
            );
        }
        String username = principal.getName();
        Integer userId = likeRecordService.getUserIdByUsername(username);
        likeRecordService.likeComment(commentId, userId);
        return ResponseEntity.ok(DataResponse.<String>builder()
                .status("success")
                .message("Bạn đã thích bình luận này thành công.")
                .build());
    }

    @DeleteMapping("/unlike/comment")
    public ResponseEntity<DataResponse<String>> unlikeComment(@RequestParam Integer commentId, Principal principal) {
        if (!commentRepository.existsById(commentId)) {  
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(
                DataResponse.<String>builder()
                    .status("error")
                    .message("Bình luận không tồn tại.")
                    .build()
            );
        }
        String username = principal.getName();
        Integer userId = likeRecordService.getUserIdByUsername(username);
        likeRecordService.unlikeComment(commentId, userId);
        return ResponseEntity.ok(DataResponse.<String>builder()
                .status("success")
                .message("Bạn đã bỏ thích bình luận này.")
                .build());
    }
    
    @GetMapping("/like-records/post")
    public ResponseEntity<DataResponse<List<LikeRecord>>> getLikeRecordByPostId(@RequestParam Integer postId) {
        List<LikeRecord> likeRecords = likeRecordService.getLikeRecordByPostId(postId);
        if (likeRecords == null || likeRecords.isEmpty()) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(
                DataResponse.<List<LikeRecord>>builder()
                    .status("error")
                    .message("Không tìm thấy danh sách thích cho bài viết này.")
                    .build()
            );
        }
        return ResponseEntity.ok(DataResponse.<List<LikeRecord>>builder()
                .status("success")
                .message("Danh sách thích của bài viết đã được lấy thành công.")
                .data(likeRecords)
                .build());
    }

    @GetMapping("/like-records/comment")
    public ResponseEntity<DataResponse<List<LikeRecord>>> getLikeRecordByCommentId(@RequestParam Integer commentId) {
        List<LikeRecord> likeRecords = likeRecordService.getLikeRecordByCommentId(commentId);
        if (likeRecords == null || likeRecords.isEmpty()) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(
                DataResponse.<List<LikeRecord>>builder()
                    .status("error")
                    .message("Không tìm thấy danh sách thích cho bình luận này.")
                    .build()
            );
        }
        return ResponseEntity.ok(DataResponse.<List<LikeRecord>>builder()
                .status("success")
                .message("Danh sách thích của bình luận đã được lấy thành công.")
                .data(likeRecords)
                .build());
    }
    
    @GetMapping("/like-count/post")
    public ResponseEntity<DataResponse<Integer>> countLikesByPostId(@RequestParam Integer postId) {
        if (!postRepository.existsById(postId)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(
                DataResponse.<Integer>builder()
                    .status("error")
                    .message("Bài viết không tồn tại.")
                    .build()
            );
        }
        Integer count = likeRecordService.countLikesByPostId(postId);
        return ResponseEntity.ok(DataResponse.<Integer>builder()
            .status("success")
            .message("Số lượt thích của bài viết đã được lấy thành công.")
            .data(count)
            .build());
    }

    @GetMapping("/like-count/comment")
    public ResponseEntity<DataResponse<Integer>> countLikesByCommentId(@RequestParam Integer commentId) {
        if (!commentRepository.existsById(commentId)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(
                DataResponse.<Integer>builder()
                    .status("error")
                    .message("Bình luận không tồn tại.")
                    .build()
            );
        }
        Integer count = likeRecordService.countLikesByCommentId(commentId);
        return ResponseEntity.ok(DataResponse.<Integer>builder()
            .status("success")
            .message("Số lượt thích của bình luận đã được lấy thành công.")
            .data(count)
            .build());
    }

}