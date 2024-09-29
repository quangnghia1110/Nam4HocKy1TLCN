package studentConsulting.controller.consultant;

import org.springframework.beans.factory.annotation.Autowired;
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
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.entity.user.RoleConsultantEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.question_answer.AnswerDTO;
import studentConsulting.model.payload.dto.notification.NotificationResponseDTO;
import studentConsulting.model.payload.request.question_answer.CreateAnswerRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.question_answer.AnswerRepository;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.common.ICommonNotificationService;
import studentConsulting.service.interfaces.consultant.IConsultantAnswerService;

import java.security.Principal;
import java.time.LocalDateTime;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class ConsultantAnswerController {

    @Autowired
    private IConsultantAnswerService answerService;

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

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @PostMapping(value = "/consultant/answer/create", consumes = {"multipart/form-data"})
    public ResponseEntity<DataResponse<AnswerDTO>> createAnswer(@RequestParam("questionId") Integer questionId,
                                                                @RequestParam("title") String title, @RequestParam("content") String content,
                                                                @RequestPart("file") MultipartFile file, @RequestParam("statusApproval") Boolean statusApproval,
                                                                Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        RoleConsultantEntity roleConsultant = user.getAccount().getRoleConsultant();

        CreateAnswerRequest answerRequest = CreateAnswerRequest.builder().questionId(questionId).title(title)
                .content(content).file(file).statusApproval(statusApproval).roleConsultantId(roleConsultant.getId())
                .consultantId(user.getId()).build();

        AnswerDTO answerDTO = answerService.createAnswer(answerRequest);
        Optional<QuestionEntity> questionOpt = questionRepository.findById(questionId);
        if (questionOpt.isEmpty()) {
            throw new ErrorException("Câu hỏi không tồn tại.");
        }

        QuestionEntity question = questionOpt.get();
        UserInformationEntity questionOwner = question.getUser();

        NotificationEntity notification = NotificationEntity.builder()
                .senderId(user.getId())
                .receiverId(questionOwner.getId())
                .content(NotificationContent.NEW_ANSWER.formatMessage(user.getLastName() + " " + user.getFirstName()))
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

        return ResponseEntity.ok(DataResponse.<AnswerDTO>builder().status("success").message("Trả lời thành công.")
                .data(answerDTO).build());
    }
}