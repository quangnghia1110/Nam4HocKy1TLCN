package studentConsulting.controller;

import java.security.Principal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;
import studentConsulting.model.entity.roleBaseAction.RoleConsultantEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.AnswerDTO;
import studentConsulting.model.payload.request.answer.CreateAnswerRequest;
import studentConsulting.model.payload.request.answer.ReviewAnswerRequest;
import studentConsulting.model.payload.request.answer.UpdateAnswerRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.AnswerRepository;
import studentConsulting.repository.QuestionRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IAnswerService;
import studentConsulting.service.INotificationService;

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

	@PreAuthorize("hasRole('TUVANVIEN')")
	@PostMapping(value = "/consultant/answer/create", consumes = { "multipart/form-data" })
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

		NotificationEntity notification = NotificationEntity.builder().senderId(user.getId())
				.receiverId(questionOwner.getId())
				.content(NotificationContent.NEW_ANSWER.formatMessage(user.getLastName() + " " + user.getFirstName()))
				.time(LocalDateTime.now()).notificationType(NotificationType.USER).status(NotificationStatus.UNREAD)
				.build();

		notificationService.sendNotification(notification);
        System.out.println("Payload: " + notification);

        simpMessagingTemplate.convertAndSendToUser(String.valueOf(questionOwner.getId()), "/notification", notification);

		return ResponseEntity.ok(DataResponse.<AnswerDTO>builder().status("success").message("Trả lời thành công.")
				.data(answerDTO).build());
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	@PreAuthorize("hasRole('TRUONGBANTUVAN')")
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

		NotificationEntity questionOwnerNotification = NotificationEntity.builder().senderId(user.getId())
				.receiverId(questionOwner.getId())
				.content(
						NotificationContent.REVIEW_ANSWER.formatMessage(user.getLastName() + " " + user.getFirstName()))
				.time(LocalDateTime.now()).notificationType(NotificationType.USER).status(NotificationStatus.UNREAD)
				.build();

		notificationService.sendNotification(questionOwnerNotification);

		String consultantContent;
		NotificationType consultantNotificationType;

		if (consultant.getAccount().getRole().getName().equals("ROLE_TUVANVIEN")) {
			consultantContent = NotificationContent.REVIEW_ANSWER_CONSULTANT
					.formatMessage(user.getLastName() + " " + user.getFirstName());
			consultantNotificationType = NotificationType.TUVANVIEN;
		} else {
			consultantContent = NotificationContent.REVIEW_ANSWER
					.formatMessage(user.getLastName() + " " + user.getFirstName());
			consultantNotificationType = NotificationType.USER;
		}

		NotificationEntity consultantNotification = NotificationEntity.builder().senderId(user.getId())
				.receiverId(consultant.getId()).content(consultantContent).time(LocalDateTime.now())
				.notificationType(consultantNotificationType).status(NotificationStatus.UNREAD).build();

		notificationService.sendNotification(consultantNotification);

		return ResponseEntity.ok(DataResponse.<AnswerDTO>builder().status("success").message("Kiểm duyệt thành công")
				.data(reviewedAnswer).build());
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	@PreAuthorize("hasRole('TRUONGBANTUVAN')")
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
	                .message("Không có câu trả lời yêu cầu p hê duyệt.")
	                .build());
	    }

	    return ResponseEntity.ok(DataResponse.<Page<AnswerDTO>>builder()
	            .status("success")
	            .message("Lấy danh sách câu trả lời đã phê duyệt thành công.")
	            .data(approvedAnswers)
	            .build());
	}
	
	@PreAuthorize("hasRole('TRUONGBANTUVAN')")
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
	
	@PreAuthorize("hasRole('TRUONGBANTUVAN')")
	@PutMapping(value="/advisor/answer/update-answer", consumes = { "multipart/form-data" })
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


	
	@PreAuthorize("hasRole('TRUONGBANTUVAN')")
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


}