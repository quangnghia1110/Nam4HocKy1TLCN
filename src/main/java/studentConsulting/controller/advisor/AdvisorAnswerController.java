package studentConsulting.controller.advisor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
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
import studentConsulting.service.interfaces.common.ICommonNotificationService;

import java.security.Principal;
import java.time.LocalDate;
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

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @PostMapping(value = "/advisor/answer/review", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<DataResponse<AnswerDTO>> reviewAnswer(@ModelAttribute ReviewAnswerRequest reviewRequest,
                                                                Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        Optional<AnswerEntity> answerOpt = answerRepository.findFirstAnswerByQuestionId(reviewRequest.getQuestionId());
        if (answerOpt.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu trả lời cho câu hỏi này.");
        }

        AnswerEntity answer = answerOpt.get();
        UserInformationEntity consultant = answer.getUser();

        if (!consultant.getAccount().getDepartment().getId().equals(user.getAccount().getDepartment().getId())) {
            throw new ErrorException("Bạn không có quyền kiểm duyệt câu trả lời từ bộ phận khác.");
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
        System.out.println("Payload: " + questionOwnerResponseDTO);

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
        System.out.println("Payload: " + consultantResponseDTO);

        simpMessagingTemplate.convertAndSendToUser(String.valueOf(consultant.getId()), "/notification", consultantResponseDTO);


        return ResponseEntity.ok(DataResponse.<AnswerDTO>builder().status("success").message("Kiểm duyệt thành công")
                .data(reviewedAnswer).build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/answer/list-answer-approved")
    public ResponseEntity<DataResponse<Page<AnswerDTO>>> getApprovedAnswers(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> managerOpt = userRepository.findUserInfoByEmail(email);
        if (!managerOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = managerOpt.get();
        Integer departmentId = manager.getAccount().getDepartment().getId();

        Page<AnswerDTO> approvedAnswers = answerService.getApprovedAnswersByDepartmentWithFilters(departmentId, startDate, endDate, page, size, sortBy, sortDir);
        if (approvedAnswers.isEmpty()) {
            return ResponseEntity.ok(DataResponse.<Page<AnswerDTO>>builder()
                    .status("success")
                    .message("Không có câu trả lời yêu cầu phê duyệt.")
                    .build());
        }

        return ResponseEntity.ok(DataResponse.<Page<AnswerDTO>>builder()
                .status("success")
                .message("Lấy danh sách câu trả lời đã phê duyệt thành công.")
                .data(approvedAnswers)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/answer/list-all-answers")
    public ResponseEntity<DataResponse<Page<AnswerDTO>>> getAllAnswersByDepartment(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> managerOpt = userRepository.findUserInfoByEmail(email);
        if (!managerOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = managerOpt.get();
        Integer departmentId = manager.getAccount().getDepartment().getId();

        Page<AnswerDTO> allAnswers = answerService.getAllAnswersByDepartmentWithFilters(departmentId, startDate, endDate, page, size, sortBy, sortDir);

        if (allAnswers.isEmpty()) {
            return ResponseEntity.ok(DataResponse.<Page<AnswerDTO>>builder()
                    .status("success")
                    .message("Không có câu trả lời nào.")
                    .build());
        }

        return ResponseEntity.ok(DataResponse.<Page<AnswerDTO>>builder()
                .status("success")
                .message("Lấy danh sách toàn bộ câu trả lời thành công.")
                .data(allAnswers)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @PutMapping(value = "/advisor/answer/update-answer", consumes = {"multipart/form-data"})
    public DataResponse<AnswerDTO> updateAnswer(
            @RequestParam("answerId") Integer answerId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestParam("statusApproval") Boolean statusApproval,
            @RequestParam("statusAnswer") Boolean statusAnswer,
            @RequestPart(value = "file", required = false) MultipartFile file,
            Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);

        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UpdateAnswerRequest answerRequest = UpdateAnswerRequest.builder()
                .title(title)
                .content(content)
                .statusApproval(statusApproval)
                .statusAnswer(statusAnswer)
                .file(file)
                .build();

        AnswerDTO updatedAnswerDTO = answerService.updateAnswer(answerId, answerRequest);

        return DataResponse.<AnswerDTO>builder()
                .status("success")
                .message("Cập nhật câu trả lời thành công.")
                .data(updatedAnswerDTO)
                .build();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @DeleteMapping("/advisor/answer/delete-answer")
    public ResponseEntity<DataResponse<Void>> deleteAnswer(@RequestParam Integer id, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> managerOpt = userRepository.findUserInfoByEmail(email);
        if (!managerOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = managerOpt.get();
        Integer departmentId = manager.getAccount().getDepartment().getId();

        answerService.deleteAnswer(id, departmentId);
        return ResponseEntity.ok(DataResponse.<Void>builder()
                .status("success")
                .message("Xóa câu trả lời thành công.")
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/answer/detail")
    public ResponseEntity<DataResponse<AnswerDTO>> getAnswerById(@RequestParam("id") Integer answerId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> managerOpt = userRepository.findUserInfoByEmail(email);
        if (!managerOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = managerOpt.get();
        Integer departmentId = manager.getAccount().getDepartment().getId();

        AnswerDTO answerDTO = answerService.getAnswerById(answerId, departmentId);
        if (answerDTO == null) {
            throw new ErrorException("Không tìm thấy câu trả lời");
        }

        return ResponseEntity.ok(DataResponse.<AnswerDTO>builder()
                .status("success")
                .message("Lấy chi tiết câu trả lời thành công.")
                .data(answerDTO)
                .build());
    }
}