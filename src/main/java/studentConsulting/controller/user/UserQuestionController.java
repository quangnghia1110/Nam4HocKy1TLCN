package studentConsulting.controller.user;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.*;
import studentConsulting.model.payload.request.question.CreateQuestionRequest;
import studentConsulting.model.payload.request.question.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.QuestionRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.interfaces.common.ICommonNotificationService;
import studentConsulting.service.interfaces.common.ICommonUserService;
import studentConsulting.service.interfaces.user.IUserQuestionService;

import java.security.Principal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@RestController
@RequestMapping("${base.url}")
public class UserQuestionController {

    @Autowired
    private IUserQuestionService questionService;

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

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @GetMapping("/user/question/list")
    public DataResponse<Page<MyQuestionDTO>> getQuestionsWithUserFilters(Principal principal,
                                                                         @RequestParam(required = false) String title, @RequestParam(required = false) Integer departmentId,
                                                                         @RequestParam(required = false) String status,
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
            try {
                filterStatus = QuestionFilterStatus.fromKey(status);
            } catch (IllegalArgumentException e) {
                throw new ErrorException("Trạng thái không hợp lệ: " + status);
            }
        }

        Page<MyQuestionDTO> questions = questionService.getQuestionsWithUserFilters(user.getId(), title,
                filterStatus != null ? filterStatus.getKey() : null, departmentId, startDate, endDate, pageable);

        if (questions == null || questions.isEmpty()) {
            throw new Exceptions.ErrorExceptionQuestion("Không tìm thấy câu hỏi nào.", "NOT_FOUND_QUESTION");
        }

        return DataResponse.<Page<MyQuestionDTO>>builder().status("success").message("Lấy câu hỏi thành công.")
                .data(questions).build();
    }

    @GetMapping("/list-question")
    public DataResponse<Page<MyQuestionDTO>> getAllQuestionsAndByDepartment(
            @RequestParam(required = false) Integer departmentId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size,
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
            throw new Exceptions.ErrorExceptionQuestion("Không tìm thấy câu hỏi nào.", "NOT_FOUND_QUESTION");
        }

        return DataResponse.<Page<MyQuestionDTO>>builder().status("success").message(
                        departmentId != null ? "Lọc câu hỏi theo phòng ban thành công." : "Lấy tất cả câu hỏi thành công.")
                .data(questions).build();
    }

    @GetMapping("/list-filter-status-options")
    public DataResponse<List<QuestionStatusDTO>> getFilterStatusOptions() {
        List<QuestionStatusDTO> statuses = Arrays.stream(QuestionFilterStatus.values())
                .map(status -> new QuestionStatusDTO(status.getKey(), status.getDisplayName()))
                .collect(Collectors.toList());

        return DataResponse.<List<QuestionStatusDTO>>builder().status("success")
                .message("Lấy tất cả trạng thái bộ lọc thành công.").data(statuses).build();
    }
}
