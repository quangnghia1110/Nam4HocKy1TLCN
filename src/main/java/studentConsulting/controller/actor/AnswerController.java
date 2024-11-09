package studentConsulting.controller.actor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.AnswerEntity;
import studentConsulting.model.entity.QuestionEntity;
import studentConsulting.model.entity.RoleConsultantEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.AnswerDTO;
import studentConsulting.model.payload.request.CreateAnswerRequest;
import studentConsulting.model.payload.request.ReviewAnswerRequest;
import studentConsulting.model.payload.request.UpdateAnswerRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.actor.AnswerRepository;
import studentConsulting.repository.actor.QuestionRepository;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.implement.common.FileStorageServiceImpl;
import studentConsulting.service.interfaces.actor.IAnswerService;
import studentConsulting.service.interfaces.common.INotificationService;

import java.security.Principal;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AnswerController {

    @Autowired
    private IAnswerService answerService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private INotificationService notificationService;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private FileStorageServiceImpl fileStorageService;

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @PostMapping(value = "/consultant/answer/create", consumes = {"multipart/form-data"})
    public ResponseEntity<DataResponse<AnswerDTO>> createAnswer(@RequestParam("questionId") Integer questionId, @RequestParam("title") String title, @RequestParam("content") String content, @RequestPart(name = "file", required = false) MultipartFile file, @RequestParam("statusApproval") Boolean statusApproval, Principal principal) {

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

        notificationService.sendUserNotification(
                user.getId(),
                questionOwner.getId(),
                NotificationContent.NEW_ANSWER.formatMessage(user.getLastName() + " " + user.getFirstName()),
                NotificationType.USER
        );

        return ResponseEntity.ok(DataResponse.<AnswerDTO>builder().status("success").message("Trả lời thành công.")
                .data(answerDTO).build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping(value = "/advisor-admin/answer/review", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<DataResponse<AnswerDTO>> reviewAnswer(
            @RequestParam("content") String content,
            @RequestParam Integer questionId,
            @RequestPart(name = "file", required = false) MultipartFile file,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        List<AnswerEntity> answers = answerRepository.findAnswersByQuestionId(questionId);
        Optional<AnswerEntity> answerOpt = answers.isEmpty() ? Optional.empty() : Optional.of(answers.get(0));
        if (answerOpt.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu trả lời cho câu hỏi này.");
        }

        AnswerEntity answer = answerOpt.get();
        UserInformationEntity consultant = answer.getUser();

        if (!isAdmin && !consultant.getAccount().getDepartment().getId().equals(user.getAccount().getDepartment().getId())) {
            throw new ErrorException("Bạn không có quyền kiểm duyệt câu trả lời");
        }

        ReviewAnswerRequest reviewRequest = ReviewAnswerRequest.builder()
                .content(content)
                .file(file)
                .build();

        if (file != null && !file.isEmpty()) {
            String fileName = fileStorageService.saveFile(file);
            answer.setFile(fileName);
        }

        answer.setContent(content);
        answerRepository.save(answer);

        AnswerDTO reviewedAnswer = answerService.reviewAnswer(questionId, reviewRequest);

        QuestionEntity question = answer.getQuestion();
        UserInformationEntity questionOwner = question.getUser();

        notificationService.sendUserNotification(
                user.getId(),
                questionOwner.getId(),
                NotificationContent.REVIEW_ANSWER.formatMessage(user.getLastName() + " " + user.getFirstName()),
                NotificationType.USER
        );

        String consultantContent = consultant.getAccount().getRole().getName().equals(SecurityConstants.Role.TUVANVIEN) ?
                NotificationContent.REVIEW_ANSWER_CONSULTANT.formatMessage(user.getLastName() + " " + user.getFirstName()) :
                NotificationContent.REVIEW_ANSWER.formatMessage(user.getLastName() + " " + user.getFirstName());

        NotificationType consultantNotificationType = consultant.getAccount().getRole().getName().equals(SecurityConstants.Role.TUVANVIEN) ?
                NotificationType.TUVANVIEN : NotificationType.USER;

        notificationService.sendUserNotification(
                user.getId(),
                consultant.getId(),
                consultantContent,
                consultantNotificationType
        );

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