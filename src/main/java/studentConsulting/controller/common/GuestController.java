package studentConsulting.controller.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.entity.LikeRecordEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.payload.dto.actor.*;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.actor.CommentRepository;
import studentConsulting.repository.actor.PostRepository;
import studentConsulting.repository.actor.QuestionRepository;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.interfaces.actor.ICommentService;
import studentConsulting.service.interfaces.actor.ILikeService;
import studentConsulting.service.interfaces.common.IGuestService;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("${base.url}")
public class GuestController {

    @Autowired
    private IGuestService guestService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommentService commentService;

    @Autowired
    private ILikeService likeRecordService;

    @Autowired
    private CommentRepository commentRepository;

    @Autowired
    private PostRepository postRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @GetMapping("/list-consultant")
    public ResponseEntity<DataResponse<Page<ConsultantDTO>>> getConsultants(
            @RequestParam(required = false) Integer departmentId,
            @RequestParam(required = false) String name,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "firstName") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConsultantDTO> consultants = guestService.getConsultant(departmentId, name, startDate, endDate, pageable);

        return ResponseEntity.ok(
                DataResponse.<Page<ConsultantDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách tư vấn viên thành công.")
                        .data(consultants)
                        .build()
        );
    }

    @GetMapping("/list-consultant-by-department")
    public ResponseEntity<DataResponse<List<UserDTO>>> getConsultantByDepartment(@RequestParam Integer departmentId) {
        List<UserDTO> consultants = guestService.getConsultantByDepartment(departmentId);

        return ResponseEntity.ok(
                DataResponse.<List<UserDTO>>builder()
                        .status("success")
                        .data(consultants)
                        .build()
        );
    }

    @GetMapping("/list-consultant-teacher-by-department")
    public ResponseEntity<DataResponse<List<UserDTO>>> getConsultantTeacherByDepartment(@RequestParam Integer departmentId) {
        List<UserDTO> consultants = guestService.getConsultantTeacherByDepartment(departmentId);

        return ResponseEntity.ok(
                DataResponse.<List<UserDTO>>builder()
                        .status("success")
                        .data(consultants)
                        .build()
        );
    }

    @GetMapping("/list-consultant-student-by-department")
    public ResponseEntity<DataResponse<List<UserDTO>>> getConsultantStudentByDepartment(@RequestParam Integer departmentId) {
        List<UserDTO> consultants = guestService.getConsultantStudentByDepartment(departmentId);

        return ResponseEntity.ok(
                DataResponse.<List<UserDTO>>builder()
                        .status("success")
                        .data(consultants)
                        .build()
        );
    }

    @GetMapping("/list-common-question")
    public ResponseEntity<DataResponse<Page<CommonQuestionDTO>>> getCommonQuestions(
            @RequestParam(required = false) Integer departmentId,
            @RequestParam(required = false) String title,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<CommonQuestionDTO> commonQuestions = guestService.getCommonQuestion(departmentId, title, startDate, endDate, pageable);

        return ResponseEntity.ok(
                DataResponse.<Page<CommonQuestionDTO>>builder()
                        .status("success")
                        .message("Lấy câu hỏi chung thành công")
                        .data(commonQuestions)
                        .build()
        );
    }

    @GetMapping("/list-department")
    public ResponseEntity<DataResponse<List<DepartmentDTO>>> getAllDepartment() {
        List<DepartmentDTO> departments = guestService.getAllDepartment();
        DataResponse<List<DepartmentDTO>> response = DataResponse.<List<DepartmentDTO>>builder()
                .status("success")
                .data(departments)
                .build();

        return ResponseEntity.ok(response);
    }

    @GetMapping("/list-field-by-department")
    public ResponseEntity<DataResponse<List<FieldDTO>>> getFieldByDepartment(@RequestParam Integer departmentId) {
        List<FieldDTO> fields = guestService.getFieldByDepartment(departmentId);
        DataResponse<List<FieldDTO>> response = DataResponse.<List<FieldDTO>>builder()
                .status("success")
                .data(fields)
                .build();

        return ResponseEntity.ok(response);
    }

    @GetMapping("/list-question")
    public DataResponse<Page<MyQuestionDTO>> getQuestion(
            @RequestParam(required = false) Integer departmentId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<MyQuestionDTO> questions = guestService.getQuestion(departmentId, startDate, endDate, pageable);

        return DataResponse.<Page<MyQuestionDTO>>builder()
                .status("success")
                .message(departmentId != null ? "Lọc câu hỏi theo phòng ban thành công." : "Lấy tất cả câu hỏi thành công.")
                .data(questions)
                .build();
    }


    @GetMapping("/list-filter-status-options")
    public DataResponse<List<QuestionStatusDTO>> getFilterStatusOptions() {
        List<QuestionStatusDTO> statuses = Arrays.stream(QuestionFilterStatus.values())
                .map(status -> new QuestionStatusDTO(status.getKey(), status.getDisplayName()))
                .collect(Collectors.toList());

        return DataResponse.<List<QuestionStatusDTO>>builder().status("success")
                .data(statuses).build();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @GetMapping("/user/question/role-ask")
    public DataResponse<List<RoleAskDTO>> getAllRoleAsk() {
        List<RoleAskDTO> roleAsks = guestService.getAllRoleAsk();
        return DataResponse.<List<RoleAskDTO>>builder().status("success")
                .data(roleAsks).build();
    }

    @GetMapping("/comment/get-comment-by-post")
    public ResponseEntity<DataResponse<List<CommentDTO>>> getCommentsByPost(@RequestParam Integer postId) {
        DataResponse<List<CommentDTO>> response = commentService.getAllComments(postId);

        return ResponseEntity.ok(response);
    }

    @GetMapping("/like-records/post")
    public ResponseEntity<DataResponse<List<LikeRecordEntity>>> getLikeRecordByPostId(@RequestParam Integer postId) {
        List<LikeRecordEntity> likeRecords = likeRecordService.getLikeRecordByPostId(postId);

        return ResponseEntity.ok(DataResponse.<List<LikeRecordEntity>>builder().status("success")
                .data(likeRecords).build());
    }

    @GetMapping("/like-records/comment")
    public ResponseEntity<DataResponse<List<LikeRecordEntity>>> getLikeRecordByCommentId(@RequestParam Integer commentId) {
        List<LikeRecordEntity> likeRecords = likeRecordService.getLikeRecordByCommentId(commentId);

        return ResponseEntity.ok(DataResponse.<List<LikeRecordEntity>>builder().status("success")
                .data(likeRecords).build());
    }

    @GetMapping("/like-count/post")
    public ResponseEntity<DataResponse<Integer>> countLikesByPostId(@RequestParam Integer postId) {
        if (!postRepository.existsById(postId)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(DataResponse.<Integer>builder().status("error").message("Bài viết không tồn tại.").build());
        }
        Integer count = likeRecordService.countLikesByPostId(postId);
        return ResponseEntity.ok(DataResponse.<Integer>builder().status("success")
                .data(count).build());
    }

    @GetMapping("/like-count/comment")
    public ResponseEntity<DataResponse<Integer>> countLikesByCommentId(@RequestParam Integer commentId) {
        if (!commentRepository.existsById(commentId)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(DataResponse.<Integer>builder().status("error").message("Bình luận không tồn tại.").build());
        }
        Integer count = likeRecordService.countLikesByCommentId(commentId);
        return ResponseEntity.ok(DataResponse.<Integer>builder().status("success")
                .data(count).build());
    }

    @GetMapping("/like-count/question")
    public ResponseEntity<DataResponse<Integer>> countLikesByQuestionId(@RequestParam Integer questionId) {
        if (!questionRepository.existsById(questionId)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(DataResponse.<Integer>builder().status("error").message("Câu hỏi không tồn tại.").build());
        }
        Integer count = likeRecordService.countLikesByQuestionId(questionId);
        return ResponseEntity.ok(DataResponse.<Integer>builder().status("success")
                .data(count).build());
    }
}
