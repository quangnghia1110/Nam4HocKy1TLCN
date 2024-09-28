package studentConsulting.controller.consultant;

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
import studentConsulting.model.payload.dto.DeletionLogDTO;
import studentConsulting.model.payload.dto.ForwardQuestionDTO;
import studentConsulting.model.payload.dto.MyQuestionDTO;
import studentConsulting.model.payload.dto.NotificationResponseDTO;
import studentConsulting.model.payload.request.question.ForwardQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.QuestionRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.interfaces.common.ICommonNotificationService;
import studentConsulting.service.interfaces.common.ICommonUserService;
import studentConsulting.service.interfaces.consultant.IConsultantQuestionService;

import java.security.Principal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class ConsultantQuestionController {

    @Autowired
    private IConsultantQuestionService questionService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommonUserService userService;

    @Autowired
    private ICommonNotificationService notificationService;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/consultant/question/list-answer")
    public DataResponse<Page<MyQuestionDTO>> getQuestionsWithConsultantFilters(Principal principal,
                                                                               @RequestParam(required = false) String title, @RequestParam(required = false) String status,
                                                                               @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
                                                                               @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
                                                                               @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size,
                                                                               @RequestParam(defaultValue = "createdAt") String sortBy,
                                                                               @RequestParam(defaultValue = "desc") String sortDir) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        QuestionFilterStatus filterStatus = null;
        if (status != null && !status.isEmpty()) {
            filterStatus = QuestionFilterStatus.fromKey(status);
        }

        Page<MyQuestionDTO> questions = questionService.getQuestionsWithConsultantFilters(user.getId(), title,
                filterStatus != null ? filterStatus.getKey() : null, startDate, endDate, pageable);

        if (questions == null || questions.isEmpty()) {
            throw new Exceptions.ErrorExceptionQuestion("Không tìm thấy câu hỏi nào.", "NOT_FOUND_QUESTION");
        }

        return DataResponse.<Page<MyQuestionDTO>>builder().status("success").message("Lấy câu hỏi thành công.")
                .data(questions).build();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/consultant/question/list-delete")
    public DataResponse<Page<DeletionLogDTO>> getDeletedQuestionsByConsultantFilters(Principal principal,
                                                                                     @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
                                                                                     @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
                                                                                     @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size,
                                                                                     @RequestParam(defaultValue = "deletedAt") String sortBy,
                                                                                     @RequestParam(defaultValue = "desc") String sortDir) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        String fullName = user.getLastName() + " " + user.getFirstName();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<DeletionLogDTO> deletedQuestions = questionService.getDeletedQuestionsByConsultantFilters(fullName,
                startDate, endDate, pageable);

        if (deletedQuestions == null || deletedQuestions.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu hỏi đã xóa.");
        }

        return DataResponse.<Page<DeletionLogDTO>>builder().status("success")
                .message("Lấy danh sách câu hỏi đã xóa thành công.").data(deletedQuestions).build();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @DeleteMapping("/consultant/question/delete")
    public DataResponse<String> deleteQuestion(@RequestParam("questionId") Integer questionId,
                                               @RequestParam("reason") String reason, Principal principal) {
        if (reason == null || reason.trim().isEmpty()) {
            throw new ErrorException("Lý do xóa là bắt buộc.");
        }

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        Optional<QuestionEntity> questionOpt = questionRepository.findById(questionId);
        if (questionOpt.isEmpty()) {
            throw new ErrorException("Câu hỏi không tồn tại.");
        }

        QuestionEntity question = questionOpt.get();
        UserInformationEntity questionOwner = question.getUser();

        questionService.deleteQuestion(questionId, reason, email);

        UserInformationEntity user = userOpt.get();

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
        System.out.println("Payload: " + responseDTO);

        simpMessagingTemplate.convertAndSendToUser(String.valueOf(questionOwner.getId()), "/notification", responseDTO);

        return DataResponse.<String>builder().status("success").message("Câu hỏi đã được xóa thành công.")
                .data("Câu hỏi đã bị xóa với lý do: " + reason).build();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @PostMapping("/consultant/question/forward")
    public DataResponse<ForwardQuestionDTO> forwardQuestion(@RequestBody ForwardQuestionRequest forwardQuestionRequest,
                                                            Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        return questionService.forwardQuestion(forwardQuestionRequest, email);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/consultant/question/list-forward")
    public DataResponse<Page<ForwardQuestionDTO>> getForwardedQuestionsByDepartmentFilters(Principal principal,
                                                                                           @RequestParam(required = false) String title, @RequestParam(required = false) Integer toDepartmentId,
                                                                                           @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
                                                                                           @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
                                                                                           @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size,
                                                                                           @RequestParam(defaultValue = "createdAt") String sortBy,
                                                                                           @RequestParam(defaultValue = "desc") String sortDir) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ForwardQuestionDTO> forwardedQuestions = questionService.getForwardedQuestionsByDepartmentFilters(title,
                toDepartmentId, startDate, endDate, pageable);

        if (forwardedQuestions == null || forwardedQuestions.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu hỏi đã chuyển tiếp.");
        }

        return DataResponse.<Page<ForwardQuestionDTO>>builder().status("success")
                .message("Lấy danh sách câu hỏi đã chuyển tiếp thành công.").data(forwardedQuestions).build();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/consultant/deletion-log/list")
    public ResponseEntity<DataResponse<Page<DeletionLogEntity>>> getDeletionLogsByConsultant(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "deletedAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        UserInformationEntity consultant = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tư vấn viên"));

        Integer consultantId = consultant.getId();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<DeletionLogEntity> logs = questionService.getDeletionLogsByConsultant(consultantId, pageable);

        return ResponseEntity.ok(DataResponse.<Page<DeletionLogEntity>>builder()
                .status("success")
                .message("Lấy lý do xóa thành công.")
                .data(logs)
                .build());
    }
}
