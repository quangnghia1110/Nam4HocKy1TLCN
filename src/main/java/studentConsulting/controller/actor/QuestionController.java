package studentConsulting.controller.actor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.question_answer.DeletionLogEntity;
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.notification.NotificationResponseDTO;
import studentConsulting.model.payload.dto.question_answer.MyQuestionDTO;
import studentConsulting.model.payload.dto.question_answer.QuestionDTO;
import studentConsulting.model.payload.dto.question_answer.QuestionStatusDTO;
import studentConsulting.model.payload.dto.user.RoleAskDTO;
import studentConsulting.model.payload.request.question_answer.CreateQuestionRequest;
import studentConsulting.model.payload.request.question_answer.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.repository.user.RoleAskRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.actor.IQuestionService;
import studentConsulting.service.interfaces.common.INotificationService;
import studentConsulting.service.interfaces.common.IUserService;

import java.security.Principal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@RestController
@RequestMapping("${base.url}")
public class QuestionController {

    @Autowired
    private IQuestionService questionService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private RoleAskRepository roleAskRepository;

    @Autowired
    private IUserService userService;

    @Autowired
    private INotificationService notificationService;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @PostMapping(value = "/user/question/create", consumes = {"multipart/form-data"})
    public DataResponse<QuestionDTO> createQuestion(Principal principal,
                                                    @RequestParam("departmentId") Integer departmentId, @RequestParam("fieldId") Integer fieldId,
                                                    @RequestParam("roleAskId") Integer roleAskId, @RequestParam("title") String title,
                                                    @RequestParam("content") String content, @RequestParam("firstName") String firstName,
                                                    @RequestParam("lastName") String lastName, @RequestParam("studentCode") String studentCode,
                                                    @RequestParam("statusPublic") Boolean statusPublic, @RequestPart("file") MultipartFile file) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        List<UserInformationEntity> consultants = userService.findConsultantsByDepartmentId(departmentId);
        if (consultants.isEmpty()) {
            throw new ErrorException("Không tìm thấy tư vấn viên nào thuộc phòng ban này.");
        }
        CreateQuestionRequest questionRequest = CreateQuestionRequest.builder().departmentId(departmentId)
                .fieldId(fieldId).roleAskId(roleAskId).title(title).content(content).firstName(firstName)
                .lastName(lastName).statusPublic(statusPublic).file(file).build();

        QuestionDTO questionDTO = questionService.createQuestion(questionRequest, user.getId()).getData();

        for (UserInformationEntity consultant : consultants) {
            NotificationEntity notification = NotificationEntity.builder()
                    .senderId(user.getId())
                    .receiverId(consultant.getId())
                    .content(NotificationContent.NEW_QUESTION.formatMessage(user.getLastName() + " " + user.getFirstName()))
                    .time(LocalDateTime.now())
                    .notificationType(NotificationType.TUVANVIEN)
                    .status(NotificationStatus.UNREAD)
                    .build();

            NotificationResponseDTO.NotificationDTO notificationDTO = NotificationResponseDTO.NotificationDTO.builder()
                    .senderId(notification.getSenderId())
                    .receiverId(notification.getReceiverId())
                    .content(notification.getContent())
                    .time(notification.getTime())
                    .notificationType(notification.getNotificationType().name())
                    .status(notification.getStatus().name())
                    .build();

            NotificationResponseDTO responseDTO = NotificationResponseDTO.builder()
                    .status("notification")
                    .data(notificationDTO)
                    .build();

            notificationService.sendNotification(notificationDTO);
            System.out.println("Payload: " + responseDTO);

            simpMessagingTemplate.convertAndSendToUser(String.valueOf(consultant.getId()), "/notification", responseDTO);

        }

        return DataResponse.<QuestionDTO>builder().status("success").message("Đặt câu hỏi thành công.")
                .data(questionDTO).build();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @PutMapping(value = "/user/question/update", consumes = {"multipart/form-data"})
    public DataResponse<QuestionDTO> updateQuestion(@RequestParam("questionId") Integer questionId,
                                                    @RequestParam("departmentId") Integer departmentId, @RequestParam("fieldId") Integer fieldId,
                                                    @RequestParam("roleAskId") Integer roleAskId, @RequestParam("title") String title,
                                                    @RequestParam("content") String content, @RequestParam("firstName") String firstName,
                                                    @RequestParam("lastName") String lastName, @RequestParam("studentCode") String studentCode,
                                                    @RequestParam("statusPublic") Boolean statusPublic,
                                                    @RequestPart(value = "file", required = false) MultipartFile file, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UpdateQuestionRequest questionRequest = UpdateQuestionRequest.builder().departmentId(departmentId)
                .fieldId(fieldId).roleAskId(roleAskId).title(title).content(content).firstName(firstName)
                .lastName(lastName).studentCode(studentCode).statusPublic(statusPublic).file(file).build();

        return questionService.updateQuestion(questionId, questionRequest);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @DeleteMapping("/user/question/delete")
    public DataResponse<Void> deleteQuestion(@RequestParam("id") Integer questionId, Principal principal) {
        String username = principal.getName();

        return questionService.deleteQuestion(questionId, username);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @PostMapping(value = "/user/question/create-follow-up", consumes = {"multipart/form-data"})
    public DataResponse<QuestionDTO> askFollowUpQuestion(Principal principal,
                                                         @RequestParam("parentQuestionId") Integer parentQuestionId, @RequestParam("title") String title,
                                                         @RequestParam("content") String content,
                                                         @RequestPart(value = "file", required = false) MultipartFile file) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        return questionService.askFollowUpQuestion(parentQuestionId, title, content, file, user.getId());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @GetMapping("/user/question/role-ask")
    public DataResponse<List<RoleAskDTO>> getAllRoleAsk() {
        List<RoleAskDTO> roleAsks = questionService.getAllRoleAsk();
        return DataResponse.<List<RoleAskDTO>>builder().status("success").message("Lấy danh sách role ask thành công.")
                .data(roleAsks).build();
    }

    @GetMapping("/list-question")
    public DataResponse<Page<MyQuestionDTO>> getAllQuestionsWithFilters(
            @RequestParam(required = false) Integer departmentId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<MyQuestionDTO> questions = questionService.getAllQuestionsWithFilters(departmentId, startDate, endDate, pageable);

        if (questions.isEmpty()) {
            throw new Exceptions.ErrorExceptionQuestion("Không tìm thấy câu hỏi nào.", "NOT_FOUND_QUESTION");
        }

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
                .message("Lấy tất cả trạng thái bộ lọc thành công.").data(statuses).build();
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/question-answer/list")
    public DataResponse<Page<MyQuestionDTO>> getQuestions(Principal principal,
                                                          @RequestParam(required = false) String title,
                                                          @RequestParam(required = false) Integer departmentId,
                                                          @RequestParam(required = false) String status,
                                                          @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
                                                          @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
                                                          @RequestParam(defaultValue = "0") int page,
                                                          @RequestParam(defaultValue = "10") int size,
                                                          @RequestParam(defaultValue = "createdAt") String sortBy,
                                                          @RequestParam(defaultValue = "desc") String sortDir) {

        String email = principal.getName();
        UserInformationEntity user = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<MyQuestionDTO> questions = questionService.getQuestionAnswerByRole(user, title, status, departmentId, startDate, endDate, pageable);

        if (questions == null || questions.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu hỏi nào.");
        }

        return DataResponse.<Page<MyQuestionDTO>>builder()
                .status("success")
                .message("Lấy câu hỏi thành công.")
                .data(questions)
                .build();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/question/delete")
    public DataResponse<String> deleteQuestion(@RequestParam("questionId") Integer questionId,
                                               @RequestParam(required = false) String reason,
                                               Principal principal) {

        if ((reason == null || reason.trim().isEmpty()) &&
                (!SecurityConstants.Role.ADMIN.equals(principal.getName()))) {
            throw new ErrorException("Lý do xóa là bắt buộc đối với tư vấn viên và trưởng ban tư vấn.");
        }

        String email = principal.getName();
        UserInformationEntity user = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));

        Optional<QuestionEntity> questionOpt = questionRepository.findById(questionId);
        if (questionOpt.isEmpty()) {
            throw new ErrorException("Câu hỏi không tồn tại.");
        }

        QuestionEntity question = questionOpt.get();
        UserInformationEntity questionOwner = question.getUser();
        Integer userDepartmentId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;
        Integer questionDepartmentId = question.getDepartment() != null ? question.getDepartment().getId() : null;

        String userRole = user.getAccount().getRole().getName();

        switch (userRole) {
            case SecurityConstants.Role.TUVANVIEN:
            case SecurityConstants.Role.TRUONGBANTUVAN:
                if (userDepartmentId == null || !userDepartmentId.equals(questionDepartmentId)) {
                    throw new ErrorException("Bạn chỉ có thể xóa câu hỏi trong phòng ban của mình.");
                }
                if (reason == null || reason.trim().isEmpty()) {
                    throw new ErrorException("Lý do xóa là bắt buộc cho vai trò của bạn.");
                }
                break;
            case SecurityConstants.Role.ADMIN:
                if (reason == null || reason.trim().isEmpty()) {
                    throw new ErrorException("Lý do xóa là bắt buộc cho vai trò của bạn.");
                }
                break;
            default:
                throw new ErrorException("Bạn không có quyền thực hiện hành động này.");
        }

        questionService.deleteQuestion(questionId, reason, email);

        if (userRole.equals(SecurityConstants.Role.TUVANVIEN) || userRole.equals(SecurityConstants.Role.TRUONGBANTUVAN)) {
            NotificationEntity notification = NotificationEntity.builder()
                    .senderId(user.getId())
                    .receiverId(questionOwner.getId())
                    .content(NotificationContent.DELETE_QUESTION.formatMessage(user.getLastName() + " " + user.getFirstName()))
                    .time(LocalDateTime.now())
                    .notificationType(NotificationType.USER)
                    .status(NotificationStatus.UNREAD)
                    .build();

            NotificationResponseDTO.NotificationDTO notificationDTO = NotificationResponseDTO.NotificationDTO.builder()
                    .senderId(notification.getSenderId())
                    .receiverId(notification.getReceiverId())
                    .content(notification.getContent())
                    .time(notification.getTime())
                    .notificationType(notification.getNotificationType().name())
                    .status(notification.getStatus().name())
                    .build();

            NotificationResponseDTO responseDTO = NotificationResponseDTO.builder()
                    .status("notification")
                    .data(notificationDTO)
                    .build();

            notificationService.sendNotification(notificationDTO);
            simpMessagingTemplate.convertAndSendToUser(String.valueOf(questionOwner.getId()), "/notification", responseDTO);
        }

        return DataResponse.<String>builder()
                .status("success")
                .message("Câu hỏi đã được xóa thành công.")
                .data("Câu hỏi đã bị xóa" + (reason != null ? " với lý do: " + reason : ""))
                .build();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/deletion-log/list")
    public ResponseEntity<DataResponse<Page<DeletionLogEntity>>> getDeletionLogs(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "deletedAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        UserInformationEntity user = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<DeletionLogEntity> logs = questionService.getDeletionLogs(user, pageable);

        return ResponseEntity.ok(
                DataResponse.<Page<DeletionLogEntity>>builder()
                        .status("success")
                        .message("Lấy lý do xóa thành công.")
                        .data(logs)
                        .build()
        );
    }
}
