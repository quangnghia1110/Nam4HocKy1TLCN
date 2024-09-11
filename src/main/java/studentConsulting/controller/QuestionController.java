package studentConsulting.controller;

import java.security.Principal;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.DeletionLogDTO;
import studentConsulting.model.payload.dto.ForwardQuestionDTO;
import studentConsulting.model.payload.dto.MyQuestionDTO;
import studentConsulting.model.payload.dto.QuestionDTO;
import studentConsulting.model.payload.dto.QuestionStatusDTO;
import studentConsulting.model.payload.dto.RoleAskDTO;
import studentConsulting.model.payload.request.question.CreateQuestionRequest;
import studentConsulting.model.payload.request.question.ForwardQuestionRequest;
import studentConsulting.model.payload.request.question.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IQuestionService;

@RestController
@RequestMapping("${base.url}")
public class QuestionController {

    @Autowired
    private IQuestionService questionService;

    @Autowired
    private UserRepository userRepository;

    @PreAuthorize("hasRole('USER')")
    @PostMapping(value = "/user/question/create", consumes = { "multipart/form-data" })
    public DataResponse<QuestionDTO> createQuestion(
        Principal principal,
        @RequestParam("departmentId") Integer departmentId,
        @RequestParam("fieldId") Integer fieldId,
        @RequestParam("roleAskId") Integer roleAskId,
        @RequestParam("title") String title,
        @RequestParam("content") String content,
        @RequestParam("firstName") String firstName,
        @RequestParam("lastName") String lastName,
        @RequestParam("studentCode") String studentCode,
        @RequestParam("statusPublic") Boolean statusPublic,
        @RequestPart("file") MultipartFile file) {

        String username = principal.getName();
        Optional<UserInformationEntity> userOptional = userRepository.findByAccountUsername(username);
        if (userOptional.isEmpty()) {
            throw new ErrorException("Không tìm thấy người dùng.");
        }

        UserInformationEntity user = userOptional.get();
        CreateQuestionRequest questionRequest = CreateQuestionRequest.builder()
                .departmentId(departmentId)
                .fieldId(fieldId)
                .roleAskId(roleAskId)
                .title(title)
                .content(content)
                .firstName(firstName)
                .lastName(lastName)
                .studentCode(studentCode)
                .statusPublic(statusPublic)
                .file(file)
                .build();

        return questionService.createQuestion(questionRequest, user.getId());
    }

    @PreAuthorize("hasRole('USER')")
    @PutMapping(value = "/user/question/update", consumes = { "multipart/form-data" })
    public DataResponse<QuestionDTO> updateQuestion(@RequestParam("questionId") Integer questionId,
        @RequestParam("departmentId") Integer departmentId, @RequestParam("fieldId") Integer fieldId,
        @RequestParam("roleAskId") Integer roleAskId, @RequestParam("title") String title,
        @RequestParam("content") String content, @RequestParam("firstName") String firstName,
        @RequestParam("lastName") String lastName, @RequestParam("studentCode") String studentCode,
        @RequestParam("statusPublic") Boolean statusPublic,
        @RequestPart(value = "file", required = false) MultipartFile file) {

        UpdateQuestionRequest questionRequest = UpdateQuestionRequest.builder()
            .departmentId(departmentId)
            .fieldId(fieldId)
            .roleAskId(roleAskId)
            .title(title)
            .content(content)
            .firstName(firstName)
            .lastName(lastName)
            .studentCode(studentCode)
            .statusPublic(statusPublic)
            .file(file)
            .build();

        return questionService.updateQuestion(questionId, questionRequest);
    }

    @PreAuthorize("hasRole('USER')")
    @DeleteMapping("/user/question/delete")
    public DataResponse<Void> deleteQuestion(@RequestParam("id") Integer questionId, Principal principal) {
        String username = principal.getName();
        return questionService.deleteQuestion(questionId, username);
    }

    @PreAuthorize("hasRole('USER')")
    @PostMapping(value = "/user/question/create-follow-up", consumes = { "multipart/form-data" })
    public DataResponse<QuestionDTO> askFollowUpQuestion(
        Principal principal, 
        @RequestParam("parentQuestionId") Integer parentQuestionId,
        @RequestParam("title") String title, @RequestParam("content") String content,
        @RequestPart(value = "file", required = false) MultipartFile file) {

        String username = principal.getName();
        Optional<UserInformationEntity> userOptional = userRepository.findByAccountUsername(username);
        if (userOptional.isEmpty()) {
            throw new ErrorException("Không tìm thấy người dùng.");
        }

        UserInformationEntity user = userOptional.get();
        return questionService.askFollowUpQuestion(parentQuestionId, title, content, file, user.getId());
    }
    
    @PreAuthorize("hasRole('USER')")
    @GetMapping("/user/question/role-ask")
    public DataResponse<List<RoleAskDTO>> getAllRoleAsk() {
        List<RoleAskDTO> roleAsks = questionService.getAllRoleAsk();
        return DataResponse.<List<RoleAskDTO>>builder()
            .status("success")
            .message("Lấy danh sách role ask thành công.")
            .data(roleAsks)
            .build();
    }
    
    @PreAuthorize("hasRole('USER')")
    @GetMapping("/user/question/list")
    public DataResponse<Page<MyQuestionDTO>> getQuestionsWithUserFilters(
        Principal principal,
        @RequestParam(required = false) String title,
        @RequestParam(required = false) Integer departmentId,
        @RequestParam(required = false) String status,
        @RequestParam(defaultValue = "0") int page,
        @RequestParam(defaultValue = "10") int size,
        @RequestParam(defaultValue = "createdAt") String sortBy,
        @RequestParam(defaultValue = "desc") String sortDir) {

        String username = principal.getName();
        Optional<UserInformationEntity> userOptional = userRepository.findByAccountUsername(username);
        if (userOptional.isEmpty()) {
            throw new ErrorException("Không tìm thấy người dùng.");
        }

        UserInformationEntity user = userOptional.get();
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        QuestionFilterStatus filterStatus = null;
        if (status != null && !status.isEmpty()) {
            try {
                filterStatus = QuestionFilterStatus.fromKey(status);
            } catch (IllegalArgumentException e) {
                throw new ErrorException("Trạng thái không hợp lệ: " + status);
            }
        }

        Page<MyQuestionDTO> questions = questionService.getQuestionsWithUserFilters(
            user.getId(), 
            title, 
            filterStatus != null ? filterStatus.getKey() : null, 
            departmentId, 
            pageable);

        if (questions == null || questions.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu hỏi nào.");
        }

        return DataResponse.<Page<MyQuestionDTO>>builder()
                .status("success")
                .message("Lấy câu hỏi thành công.")
                .data(questions)
                .build();
    }
    
    @GetMapping("/list-question")
    public DataResponse<Page<MyQuestionDTO>> getAllQuestionsAndByDepartment(
        @RequestParam(required = false) Integer departmentId,
        @RequestParam(defaultValue = "0") int page,
        @RequestParam(defaultValue = "10") int size,
        @RequestParam(defaultValue = "createdAt") String sortBy,
        @RequestParam(defaultValue = "desc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<MyQuestionDTO> questions;
        if (departmentId != null) {
            questions = questionService.getAllQuestionsByDepartmentFilters(departmentId, pageable);
        } else {
            questions = questionService.getAllQuestionsFilters(pageable);
        }

        if (questions.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu hỏi nào.");
        }

        return DataResponse.<Page<MyQuestionDTO>>builder()
                .status("success")
                .message(departmentId != null ? "Lọc câu hỏi theo phòng ban thành công." : "Lấy tất cả câu hỏi thành công.")
                .data(questions)
                .build();
    }
    
    @PreAuthorize("hasRole('TUVANVIEN')")
    @GetMapping("/consultant/question/list-answer")
    public DataResponse<Page<MyQuestionDTO>> getQuestionsWithConsultantFilters(
        Principal principal,
        @RequestParam(required = false) String title,
        @RequestParam(required = false) String status,
        @RequestParam(defaultValue = "0") int page,
        @RequestParam(defaultValue = "10") int size,
        @RequestParam(defaultValue = "createdAt") String sortBy,
        @RequestParam(defaultValue = "desc") String sortDir) {

        String username = principal.getName();
        Optional<UserInformationEntity> userOptional = userRepository.findByAccountUsername(username);
        if (userOptional.isEmpty()) {
            throw new ErrorException("Không tìm thấy người dùng.");
        }

        UserInformationEntity user = userOptional.get();
        String roleName = user.getAccount().getRole().getName();
        if (!roleName.equals("TUVANVIEN")) {
            throw new ErrorException("Bạn không có quyền truy cập tài nguyên này.");
        }

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        QuestionFilterStatus filterStatus = null;
        if (status != null && !status.isEmpty()) {
            filterStatus = QuestionFilterStatus.fromKey(status);
        }

        Page<MyQuestionDTO> questions = questionService.getQuestionsWithConsultantFilters(
            user.getId(), title, filterStatus != null ? filterStatus.getKey() : null, pageable);

        if (questions == null || questions.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu hỏi nào.");
        }

        return DataResponse.<Page<MyQuestionDTO>>builder()
            .status("success")
            .message("Lấy câu hỏi thành công.")
            .data(questions)
            .build();
    }

    @PreAuthorize("hasRole('TUVANVIEN')")
    @GetMapping("/consultant/question/list-delete")
    public DataResponse<Page<DeletionLogDTO>> getDeletedQuestionsByConsultantFilters(
        Principal principal,
        @RequestParam(defaultValue = "0") int page,
        @RequestParam(defaultValue = "10") int size,
        @RequestParam(defaultValue = "deletedAt") String sortBy,
        @RequestParam(defaultValue = "desc") String sortDir) {

        String username = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findByAccountUsername(username);
        if (userOpt.isEmpty()) {
            throw new ErrorException("Không tìm thấy người dùng.");
        }

        UserInformationEntity user = userOpt.get();
        String fullName = user.getLastName() + " " + user.getFirstName();

        if (!isConsultant(user)) {
            throw new ErrorException("Bạn không có quyền truy cập tài nguyên này.");
        }
        
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<DeletionLogDTO> deletedQuestions = questionService.getDeletedQuestionsByConsultantFilters(fullName, pageable);

        if (deletedQuestions == null || deletedQuestions.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu hỏi đã xóa.");
        }

        return DataResponse.<Page<DeletionLogDTO>>builder()
                .status("success")
                .message("Lấy danh sách câu hỏi đã xóa thành công.")
                .data(deletedQuestions)
                .build();
    }
    
    @PreAuthorize("hasRole('TUVANVIEN')")
    @DeleteMapping("/consultant/question/delete")
    public DataResponse<String> deleteQuestion(@RequestParam("questionId") Integer questionId,@RequestParam("reason") String reason,Principal principal) {
        if (reason == null || reason.trim().isEmpty()) {
            throw new ErrorException("Lý do xóa là bắt buộc.");
        }

        String username = principal.getName();
        return questionService.deleteQuestion(questionId, reason, username);
    }

    @PreAuthorize("hasRole('TUVANVIEN')")
    @PostMapping("/consultant/question/forward")
    public DataResponse<ForwardQuestionDTO> forwardQuestion(
        @RequestBody ForwardQuestionRequest forwardQuestionRequest, Principal principal) {
        
        String username = principal.getName();
        return questionService.forwardQuestion(forwardQuestionRequest, username);
    }

    @PreAuthorize("hasRole('TUVANVIEN')")
    @GetMapping("/consultant/question/list-forward")
    public DataResponse<Page<ForwardQuestionDTO>> getForwardedQuestionsByDepartmentFilters(
        Principal principal,
        @RequestParam(required = false) String title,
        @RequestParam(required = false) Integer toDepartmentId,
        @RequestParam(defaultValue = "0") int page,
        @RequestParam(defaultValue = "10") int size,
        @RequestParam(defaultValue = "createdAt") String sortBy,
        @RequestParam(defaultValue = "desc") String sortDir) {

        String username = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findByAccountUsername(username);
        if (userOpt.isEmpty()) {
            throw new ErrorException("Không tìm thấy người dùng.");
        }

        UserInformationEntity user = userOpt.get();
        if (!isConsultant(user)) {
            throw new ErrorException("Bạn không có quyền truy cập tài nguyên này.");
        }

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ForwardQuestionDTO> forwardedQuestions = questionService.getForwardedQuestionsByDepartmentFilters(title, toDepartmentId, pageable);

        if (forwardedQuestions == null || forwardedQuestions.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu hỏi đã chuyển tiếp.");
        }

        return DataResponse.<Page<ForwardQuestionDTO>>builder()
                .status("success")
                .message("Lấy danh sách câu hỏi đã chuyển tiếp thành công.")
                .data(forwardedQuestions)
                .build();
    }

    @PreAuthorize("hasRole('TRUONGBANTUVAN')")
    @GetMapping("/advisor/question/list-question-by-department")
    public DataResponse<Page<MyQuestionDTO>> getDepartmentConsultantsQuestionsFilters(
        Principal principal,
        @RequestParam(required = false) String title,
        @RequestParam(required = false) String status,
        @RequestParam(defaultValue = "0") int page,
        @RequestParam(defaultValue = "10") int size,
        @RequestParam(defaultValue = "createdAt") String sortBy,
        @RequestParam(defaultValue = "desc") String sortDir) {

        String username = principal.getName();
        Optional<UserInformationEntity> userOptional = userRepository.findByAccountUsername(username);
        if (userOptional.isEmpty()) {
            throw new ErrorException("Không tìm thấy người dùng.");
        }

        UserInformationEntity user = userOptional.get();
        String roleName = user.getAccount().getRole().getName();
        if (!roleName.equals("TRUONGBANTUVAN")) {
            throw new ErrorException("Bạn không có quyền truy cập tài nguyên này.");
        }

        Integer departmentId = user.getAccount().getDepartment().getId();
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        QuestionFilterStatus filterStatus = null;
        if (status != null && !status.isEmpty()) {
            filterStatus = QuestionFilterStatus.fromKey(status);
        }

        Page<MyQuestionDTO> questions = questionService.getDepartmentConsultantsQuestionsFilters(
            departmentId, title, filterStatus != null ? filterStatus.getKey() : null, pageable);

        if (questions == null || questions.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu hỏi.");
        }

        return DataResponse.<Page<MyQuestionDTO>>builder()
                .status("success")
                .message("Lấy danh sách câu hỏi thành công.")
                .data(questions)
                .build();
    }

    private boolean isConsultant(UserInformationEntity user) {
        return "TUVANVIEN".equals(user.getAccount().getRole().getName());
    }
    
    
    @GetMapping("/list-filter-status-options")
    public DataResponse<List<QuestionStatusDTO>> getFilterStatusOptions() {
        List<QuestionStatusDTO> statuses = Arrays.stream(QuestionFilterStatus.values())
                .map(status -> new QuestionStatusDTO(status.getKey(), status.getDisplayName()))
                .collect(Collectors.toList());

        return DataResponse.<List<QuestionStatusDTO>>builder()
                .status("success")
                .message("Lấy tất cả trạng thái bộ lọc thành công.")
                .data(statuses)
                .build();
    }
}
