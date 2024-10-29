package studentConsulting.controller.advisor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.question_answer.AnswerEntity;
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.notification.NotificationResponseDTO;
import studentConsulting.model.payload.dto.question_answer.AnswerDTO;
import studentConsulting.model.payload.request.question_answer.ReviewAnswerRequest;
import studentConsulting.model.payload.request.question_answer.UpdateAnswerRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.question_answer.AnswerRepository;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorAnswerService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonNotificationService;
import studentConsulting.service.interfaces.common.ICommonPdfService;

import java.security.Principal;
import java.time.LocalDateTime;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdvisorAnswerController {

    @Autowired
    private IAdvisorAnswerService answerService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommonNotificationService notificationService;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping(value = "/advisor-admin/answer/review", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<DataResponse<AnswerDTO>> reviewAnswer(@ModelAttribute ReviewAnswerRequest reviewRequest,
                                                                Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        Optional<AnswerEntity> answerOpt = answerRepository.findFirstAnswerByQuestionId(reviewRequest.getQuestionId());
        if (answerOpt.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu trả lời cho câu hỏi này.");
        }

        AnswerEntity answer = answerOpt.get();
        UserInformationEntity consultant = answer.getUser();

        if (!isAdmin && !consultant.getAccount().getDepartment().getId().equals(user.getAccount().getDepartment().getId())) {
            throw new ErrorException("Bạn không có quyền kiểm duyệt câu trả lời");
        }

        AnswerDTO reviewedAnswer = answerService.reviewAnswer(reviewRequest);

        QuestionEntity question = answer.getQuestion();
        UserInformationEntity questionOwner = question.getUser();

        NotificationEntity questionOwnerNotification = NotificationEntity.builder()
                .senderId(user.getId())
                .receiverId(questionOwner.getId())
                .content(NotificationContent.REVIEW_ANSWER.formatMessage(user.getLastName() + " " + user.getFirstName()))
                .time(LocalDateTime.now())
                .notificationType(NotificationType.USER)
                .status(NotificationStatus.UNREAD)
                .build();

        NotificationResponseDTO.NotificationDTO questionOwnerNotificationDTO = NotificationResponseDTO.NotificationDTO.builder()
                .senderId(questionOwnerNotification.getSenderId())
                .receiverId(questionOwnerNotification.getReceiverId())
                .content(questionOwnerNotification.getContent())
                .time(questionOwnerNotification.getTime())
                .notificationType(questionOwnerNotification.getNotificationType().name())
                .status(questionOwnerNotification.getStatus().name())
                .build();

        NotificationResponseDTO questionOwnerResponseDTO = NotificationResponseDTO.builder()
                .status("notification")
                .data(questionOwnerNotificationDTO)
                .build();

        notificationService.sendNotification(questionOwnerNotificationDTO);
        simpMessagingTemplate.convertAndSendToUser(String.valueOf(questionOwner.getId()), "/notification", questionOwnerResponseDTO);

        String consultantContent = consultant.getAccount().getRole().getName().equals(SecurityConstants.Role.TUVANVIEN) ?
                NotificationContent.REVIEW_ANSWER_CONSULTANT.formatMessage(user.getLastName() + " " + user.getFirstName()) :
                NotificationContent.REVIEW_ANSWER.formatMessage(user.getLastName() + " " + user.getFirstName());

        NotificationType consultantNotificationType = consultant.getAccount().getRole().getName().equals(SecurityConstants.Role.TUVANVIEN) ?
                NotificationType.TUVANVIEN : NotificationType.USER;

        NotificationEntity consultantNotification = NotificationEntity.builder()
                .senderId(user.getId())
                .receiverId(consultant.getId())
                .content(consultantContent)
                .time(LocalDateTime.now())
                .notificationType(consultantNotificationType)
                .status(NotificationStatus.UNREAD)
                .build();

        NotificationResponseDTO.NotificationDTO consultantNotificationDTO = NotificationResponseDTO.NotificationDTO.builder()
                .senderId(consultantNotification.getSenderId())
                .receiverId(consultantNotification.getReceiverId())
                .content(consultantNotification.getContent())
                .time(consultantNotification.getTime())
                .notificationType(consultantNotification.getNotificationType().name())
                .status(consultantNotification.getStatus().name())
                .build();

        NotificationResponseDTO consultantResponseDTO = NotificationResponseDTO.builder()
                .status("notification")
                .data(consultantNotificationDTO)
                .build();

        notificationService.sendNotification(consultantNotificationDTO);
        simpMessagingTemplate.convertAndSendToUser(String.valueOf(consultant.getId()), "/notification", consultantResponseDTO);

        return ResponseEntity.ok(DataResponse.<AnswerDTO>builder().status("success").message("Kiểm duyệt thành công")
                .data(reviewedAnswer).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping(value = "/answer/update", consumes = {"multipart/form-data"})
    public DataResponse<AnswerDTO> updateAnswer(
            @RequestParam("answerId") Integer answerId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestParam("statusApproval") Boolean statusApproval,
            @RequestParam("statusAnswer") Boolean statusAnswer,
            @RequestPart(value = "file", required = false) MultipartFile file,
            Principal principal) {

        String email = principal.getName();
        UserInformationEntity user = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));

        UpdateAnswerRequest answerRequest = UpdateAnswerRequest.builder()
                .title(title)
                .content(content)
                .statusApproval(statusApproval)
                .statusAnswer(statusAnswer)
                .file(file)
                .build();

        return DataResponse.<AnswerDTO>builder()
                .status("success")
                .message("Cập nhật câu trả lời thành công.")
                .data(answerService.updateAnswer(answerId, answerRequest, user))
                .build();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/answer/delete")
    public ResponseEntity<DataResponse<Void>> deleteAnswer(@RequestParam("id") Integer id, Principal principal) {
        String email = principal.getName();
        UserInformationEntity user = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));

        answerService.deleteAnswer(id, user);

        return ResponseEntity.ok(DataResponse.<Void>builder()
                .status("success")
                .message("Xóa câu trả lời thành công.")
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/answer/detail")
    public ResponseEntity<DataResponse<AnswerDTO>> getAnswerById(@RequestParam("id") Integer answerId, Principal principal) {
        String email = principal.getName();
        UserInformationEntity user = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));

        AnswerDTO answerDTO = answerService.getAnswerById(answerId, user);

        return ResponseEntity.ok(DataResponse.<AnswerDTO>builder()
                .status("success")
                .message("Lấy chi tiết câu trả lời thành công.")
                .data(answerDTO)
                .build());
    }
}