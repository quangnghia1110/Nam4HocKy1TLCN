package studentConsulting.controller.consultant;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.notification.NotificationResponseDTO;
import studentConsulting.model.payload.dto.question_answer.MyQuestionDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.repository.user.UserRepository;
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
}
