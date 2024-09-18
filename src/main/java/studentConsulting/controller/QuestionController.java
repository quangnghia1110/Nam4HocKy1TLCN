package studentConsulting.controller;

import java.security.Principal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
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

import studentConsulting.constant.SecurityService;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;
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
import studentConsulting.repository.QuestionRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.INotificationService;
import studentConsulting.service.IQuestionService;
import studentConsulting.service.IUserService;

@RestController
@RequestMapping("${base.url}")
public class QuestionController {

    @Autowired
    private IQuestionService questionService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private IUserService userService;
    
    @Autowired
    private INotificationService notificationService;  
    
    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private SecurityService securityService;
    
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
        Optional<UserInformationEntity> userOpt = securityService.getAuthenticatedUser(username, userRepository);

        UserInformationEntity user = userOpt.get();
        List<UserInformationEntity> consultants = userService.findConsultantsByDepartmentId(departmentId);
        if (consultants.isEmpty()) {
            throw new ErrorException("Không tìm thấy tư vấn viên nào thuộc phòng ban này.");
        }
        CreateQuestionRequest questionRequest = CreateQuestionRequest.builder()
                .departmentId(departmentId)
                .fieldId(fieldId)
                .roleAskId(roleAskId)
                .title(title)
                .content(content)
                .firstName(firstName)
                .lastName(lastName)
                .statusPublic(statusPublic)
                .file(file)
                .build();

        QuestionDTO questionDTO = questionService.createQuestion(questionRequest, user.getId()).getData();
        
        

        for (UserInformationEntity consultant : consultants) {
            NotificationEntity notification = NotificationEntity.builder()
                    .senderId(user.getId())
                    .receiverId(consultant.getId())
                    .content(NotificationContent.NEW_QUESTION.formatMessage(user.getLastName() + " " + user.getFirstName()))                    .time(LocalDateTime.now())
                    .notificationType(NotificationType.TUVANVIEN)
                    .status(NotificationStatus.UNREAD)
                    .build();

            notificationService.sendNotification(notification);
        }

        return DataResponse.<QuestionDTO>builder()
                .status("success")
                .message("Đặt câu hỏi thành công.")
                .data(questionDTO)
                .build();
    }


    @PreAuthorize("hasRole('USER')")
    @PutMapping(value = "/user/question/update", consumes = { "multipart/form-data" })
    public DataResponse<QuestionDTO> updateQuestion(
            @RequestParam("questionId") Integer questionId,
            @RequestParam("departmentId") Integer departmentId, 
            @RequestParam("fieldId") Integer fieldId,
            @RequestParam("roleAskId") Integer roleAskId, 
            @RequestParam("title") String title,
            @RequestParam("content") String content, 
            @RequestParam("firstName") String firstName,
            @RequestParam("lastName") String lastName, 
            @RequestParam("studentCode") String studentCode,
            @RequestParam("statusPublic") Boolean statusPublic,
            @RequestPart(value = "file", required = false) MultipartFile file,
            Principal principal) {
       
        String username = principal.getName();

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
        Optional<UserInformationEntity> userOpt = securityService.getAuthenticatedUser(username, userRepository);

        UserInformationEntity user = userOpt.get();
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
        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
        @RequestParam(defaultValue = "0") int page,
        @RequestParam(defaultValue = "10") int size,
        @RequestParam(defaultValue = "createdAt") String sortBy,
        @RequestParam(defaultValue = "desc") String sortDir) {

        String username = principal.getName();
        System.out.println("Username: " + username); 
        Optional<UserInformationEntity> userOpt = securityService.getAuthenticatedUser(username, userRepository);

        UserInformationEntity user = userOpt.get();
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
            startDate, 
            endDate, 
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
        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
        @RequestParam(defaultValue = "0") int page,
        @RequestParam(defaultValue = "10") int size,
        @RequestParam(defaultValue = "createdAt") String sortBy,
        @RequestParam(defaultValue = "desc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<MyQuestionDTO> questions;
        if (departmentId != null) {
            questions = questionService.getAllQuestionsByDepartmentFilters(departmentId, startDate, endDate, pageable);
        } else {
            questions = questionService.getAllQuestionsFilters(startDate, endDate, pageable);
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
        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
        @RequestParam(defaultValue = "0") int page,
        @RequestParam(defaultValue = "10") int size,
        @RequestParam(defaultValue = "createdAt") String sortBy,
        @RequestParam(defaultValue = "desc") String sortDir) {

        String username = principal.getName();
        Optional<UserInformationEntity> userOpt = securityService.getAuthenticatedUser(username, userRepository);

        UserInformationEntity user = userOpt.get();       

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        QuestionFilterStatus filterStatus = null;
        if (status != null && !status.isEmpty()) {
            filterStatus = QuestionFilterStatus.fromKey(status);
        }

        Page<MyQuestionDTO> questions = questionService.getQuestionsWithConsultantFilters(
            user.getId(), title, filterStatus != null ? filterStatus.getKey() : null, startDate, endDate, pageable);

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
        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
        @RequestParam(defaultValue = "0") int page,
        @RequestParam(defaultValue = "10") int size,
        @RequestParam(defaultValue = "deletedAt") String sortBy,
        @RequestParam(defaultValue = "desc") String sortDir) {

        String username = principal.getName();
        Optional<UserInformationEntity> userOpt = securityService.getAuthenticatedUser(username, userRepository);


        UserInformationEntity user = userOpt.get();
        String fullName = user.getLastName() + " " + user.getFirstName();

        
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<DeletionLogDTO> deletedQuestions = questionService.getDeletedQuestionsByConsultantFilters(fullName,startDate, endDate, pageable);

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
        
        Optional<QuestionEntity> questionOpt = questionRepository.findById(questionId);
        if (questionOpt.isEmpty()) {
            throw new ErrorException("Câu hỏi không tồn tại.");
        }
        
        QuestionEntity question = questionOpt.get();
        UserInformationEntity questionOwner = question.getUser();

        questionService.deleteQuestion(questionId, reason, username);

        Optional<UserInformationEntity> userOpt = securityService.getAuthenticatedUser(username, userRepository);

        UserInformationEntity user = userOpt.get();

        NotificationEntity notification = NotificationEntity.builder()
                .senderId(user.getId())
                .receiverId(questionOwner.getId())
                .content(NotificationContent.DELETE_QUESTION.formatMessage(user.getLastName() + " " + user.getFirstName()))                .time(LocalDateTime.now())
                .notificationType(NotificationType.USER)
                .status(NotificationStatus.UNREAD)
                .build();
        
        notificationService.sendNotification(notification);

        return DataResponse.<String>builder()
                .status("success")
                .message("Câu hỏi đã được xóa thành công.")
                .data("Câu hỏi đã bị xóa với lý do: " + reason)
                .build();
    }

    @PreAuthorize("hasRole('TUVANVIEN')")
    @PostMapping("/consultant/question/forward")
    public DataResponse<ForwardQuestionDTO> forwardQuestion(
        @RequestBody ForwardQuestionRequest forwardQuestionRequest, Principal principal) {
        
        String username = principal.getName();
        Optional<UserInformationEntity> userOpt = securityService.getAuthenticatedUser(username, userRepository);

        return questionService.forwardQuestion(forwardQuestionRequest, username);
    }

    @PreAuthorize("hasRole('TUVANVIEN')")
    @GetMapping("/consultant/question/list-forward")
    public DataResponse<Page<ForwardQuestionDTO>> getForwardedQuestionsByDepartmentFilters(
        Principal principal,
        @RequestParam(required = false) String title,
        @RequestParam(required = false) Integer toDepartmentId,
        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
        @RequestParam(defaultValue = "0") int page,
        @RequestParam(defaultValue = "10") int size,
        @RequestParam(defaultValue = "createdAt") String sortBy,
        @RequestParam(defaultValue = "desc") String sortDir) {

        String username = principal.getName();
        Optional<UserInformationEntity> userOpt = securityService.getAuthenticatedUser(username, userRepository);

        UserInformationEntity user = userOpt.get();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ForwardQuestionDTO> forwardedQuestions = questionService.getForwardedQuestionsByDepartmentFilters(title, toDepartmentId,startDate, endDate, pageable);

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
        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
        @RequestParam(defaultValue = "0") int page,
        @RequestParam(defaultValue = "10") int size,
        @RequestParam(defaultValue = "createdAt") String sortBy,
        @RequestParam(defaultValue = "desc") String sortDir) {

        String username = principal.getName();
        Optional<UserInformationEntity> userOpt = securityService.getAuthenticatedUser(username, userRepository);

        UserInformationEntity user = userOpt.get();
        
        Integer departmentId = user.getAccount().getDepartment().getId();
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        QuestionFilterStatus filterStatus = null;
        if (status != null && !status.isEmpty()) {
            filterStatus = QuestionFilterStatus.fromKey(status);
        }

        Page<MyQuestionDTO> questions = questionService.getDepartmentConsultantsQuestionsFilters(
            departmentId, title, filterStatus != null ? filterStatus.getKey() : null,startDate, endDate, pageable);

        if (questions == null || questions.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu hỏi.");
        }

        return DataResponse.<Page<MyQuestionDTO>>builder()
                .status("success")
                .message("Lấy danh sách câu hỏi thành công.")
                .data(questions)
                .build();
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
