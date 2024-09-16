package studentConsulting.controller;

import java.security.Principal;
import java.time.LocalDate;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import studentConsulting.constant.SecurityService;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.UserType;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;
import studentConsulting.model.entity.roleBaseAction.RoleConsultantEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.AnswerDTO;
import studentConsulting.model.payload.request.answer.CreateAnswerRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.AnswerRepository;
import studentConsulting.repository.QuestionRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IAnswerService;
import studentConsulting.service.INotificationService;
import studentConsulting.service.IQuestionService;
import studentConsulting.service.IUserService;

@RestController
@RequestMapping("${base.url}")
public class AnswerController {

    @Autowired
    private IAnswerService answerService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private AnswerRepository answerRepository;
    
    @Autowired
    private INotificationService notificationService;  
    
    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private SecurityService securityService;
    
    @PreAuthorize("hasRole('TUVANVIEN')")
    @PostMapping(value = "/consultant/answer/create", consumes = { "multipart/form-data" })
    public ResponseEntity<DataResponse<AnswerDTO>> createAnswer(
            @RequestParam("questionId") Integer questionId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestPart("file") MultipartFile file,
            @RequestParam("statusApproval") Boolean statusApproval,
            Principal principal) {

        String username = principal.getName();

        Optional<UserInformationEntity> userOpt = securityService.getAuthenticatedUser(username, userRepository);

        UserInformationEntity user = userOpt.get();
        RoleConsultantEntity roleConsultant = user.getAccount().getRoleConsultant();
        
        CreateAnswerRequest answerRequest = CreateAnswerRequest.builder()
                .questionId(questionId)
                .title(title)
                .content(content)
                .file(file)
                .statusApproval(statusApproval)
                .roleConsultantId(roleConsultant.getId())
                .consultantId(user.getId())
                .build();

        AnswerDTO answerDTO = answerService.createAnswer(answerRequest);
        Optional<QuestionEntity> questionOpt = questionRepository.findById(questionId);
        if (questionOpt.isEmpty()) {
            throw new ErrorException("Câu hỏi không tồn tại.");
        }

        QuestionEntity question = questionOpt.get();
        UserInformationEntity questionOwner = question.getUser();
        String messageContent = "Bạn có câu trả lời mới từ  " 
                + user.getLastName() + " " + user.getFirstName();
        NotificationEntity notification = NotificationEntity.builder()
                .senderId(user.getId())
                .receiverId(questionOwner.getId())
                .content(messageContent)
                .time(LocalDate.now())
                .userType(UserType.USER)
                .status(NotificationStatus.UNREAD)
                .build();

        notificationService.sendNotification(notification);
        return ResponseEntity.ok(DataResponse.<AnswerDTO>builder()
                .status("success")
                .message("Trả lời thành công.")
                .data(answerDTO)
                .build());
    }

    @PreAuthorize("hasRole('TRUONGBANTUVAN')")
    @PostMapping("/advisor/answer/review")
    public ResponseEntity<DataResponse<AnswerDTO>> reviewAnswer(
            @RequestBody CreateAnswerRequest reviewRequest, Principal principal) {

        String username = principal.getName();

        Optional<UserInformationEntity> userOpt = securityService.getAuthenticatedUser(username, userRepository);

        UserInformationEntity user = userOpt.get();

        Optional<AnswerEntity> answerOpt = answerRepository.findFirstAnswerByQuestionId(reviewRequest.getQuestionId());
        if (answerOpt.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu trả lời cho câu hỏi này.");
        }

        AnswerEntity answer = answerOpt.get();

        if (answer.getStatusAnswer() != null && answer.getStatusAnswer()) {
            throw new ErrorException("Câu trả lời này đã được duyệt và không thể kiểm duyệt lại.");
        }

        UserInformationEntity consultant = answer.getUser();

        if (!consultant.getAccount().getDepartment().getId().equals(user.getAccount().getDepartment().getId())) {
            throw new ErrorException("Bạn không có quyền kiểm duyệt câu trả lời từ bộ phận khác.");
        }

        AnswerDTO reviewedAnswer = answerService.reviewAnswer(reviewRequest);

        QuestionEntity question = answer.getQuestion();
        UserInformationEntity questionOwner = question.getUser();  

        String messageContent = "Bạn có câu trả lời mới từ  " 
                                + user.getLastName() + " " + user.getFirstName();

        NotificationEntity notification = NotificationEntity.builder()
                .senderId(user.getId()) 
                .receiverId(questionOwner.getId())  
                .content(messageContent)
                .time(LocalDate.now())
                .userType(UserType.USER)
                .status(NotificationStatus.UNREAD)
                .build();

        notificationService.sendNotification(notification);

        return ResponseEntity.ok(DataResponse.<AnswerDTO>builder()
            .status("success")
            .message("Kiểm duyệt thành công")
            .data(reviewedAnswer)
            .build());
    }
}