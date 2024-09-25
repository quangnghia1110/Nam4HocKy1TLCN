package studentConsulting.controller;

import java.security.Principal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationStatus;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.request.consultant.ConsultationFeedbackRequest;
import studentConsulting.model.payload.request.consultant.CreateScheduleConsultationRequest;
import studentConsulting.model.payload.request.consultant.ManageCreateConsultantScheduleRequest;
import studentConsulting.model.payload.request.consultant.UpdateConsultationScheduleRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.ConsultationScheduleRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IConsultationScheduleService;
import studentConsulting.service.INotificationService;
import studentConsulting.service.IUserService;

@RestController
@RequestMapping("${base.url}")
public class ConsultationScheduleController {

	@Autowired
	private IConsultationScheduleService consultationScheduleService;

	@Autowired
	private IUserService userService;

	@Autowired
	private INotificationService notificationService;

	@Autowired
	private UserRepository userRepository;
	
	@Autowired
	private ConsultationScheduleRepository consultationScheduleRepository;

	@Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

	@PreAuthorize("hasRole('USER')")
	@PostMapping("/user/consultation-schedule/create")
	public ResponseEntity<DataResponse<ConsultationScheduleDTO>> createConsultation(
			@RequestBody CreateScheduleConsultationRequest request, Principal principal) {

		String email = principal.getName();
		System.out.println("Email: " + email);
		Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
		if (!userOpt.isPresent()) {
			throw new ErrorException("Không tìm thấy người dùng");
		}
		UserInformationEntity user = userOpt.get();

		ConsultationScheduleDTO createdSchedule = consultationScheduleService.createConsultation(request, user);

		UserInformationEntity consultant = userService.findConsultantById(request.getConsultantId())
				.orElseThrow(() -> new ErrorException("Tư vấn viên không tồn tại"));

		NotificationEntity notification = NotificationEntity.builder().senderId(user.getId())
				.receiverId(consultant.getId())
				.content(NotificationContent.NEW_CONSULATION_SCHEDULE
						.formatMessage(user.getLastName() + " " + user.getFirstName()))
				.time(LocalDateTime.now()).notificationType(NotificationType.TUVANVIEN)
				.status(NotificationStatus.UNREAD).build();

		notificationService.sendNotification(notification);
        System.out.println("Payload: " + notification);
        simpMessagingTemplate.convertAndSendToUser(String.valueOf(consultant.getId()), "/private", notification);

		return ResponseEntity.ok(DataResponse.<ConsultationScheduleDTO>builder().status("success")
				.message("Lịch tư vấn đã được tạo thành công.").data(createdSchedule).build());
	}

	@PreAuthorize("hasRole('USER')")
	@GetMapping("/user/consultation-schedule/list")
	public ResponseEntity<DataResponse<Page<ConsultationScheduleDTO>>> getFilterScheduleByUser(
			@RequestParam(required = false) Integer departmentId, @RequestParam(required = false) String title,
			@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
			@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
			@RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size,
			@RequestParam(defaultValue = "title") String sortBy, @RequestParam(defaultValue = "asc") String sortDir,
			Principal principal) {

		String email = principal.getName();
		System.out.println("Email: " + email);
		Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
		if (!userOpt.isPresent()) {
			throw new ErrorException("Không tìm thấy người dùng");
		}
		UserInformationEntity user = userOpt.get();

		Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
		Page<ConsultationScheduleDTO> schedules = consultationScheduleService.getSchedulesByUserWithFilters(user,
				departmentId, title, startDate, endDate, pageable);

		if (schedules.isEmpty()) {
			return ResponseEntity.status(404).body(DataResponse.<Page<ConsultationScheduleDTO>>builder().status("error")
					.message("Không tìm thấy lịch tư vấn.").build());
		}

		return ResponseEntity.ok(DataResponse.<Page<ConsultationScheduleDTO>>builder().status("success")
				.message("Lấy danh sách lịch tư vấn thành công.").data(schedules).build());
	}

	@PreAuthorize("hasRole('TUVANVIEN')")
	@GetMapping("/consultant/consultation-schedule/list")
	public ResponseEntity<DataResponse<Page<ConsultationScheduleDTO>>> getConsultationSchedulesByConsultant(
			@RequestParam(required = false) String title, @RequestParam(required = false) Boolean statusPublic,
			@RequestParam(required = false) Boolean statusConfirmed, @RequestParam(required = false) Boolean mode,
			@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
			@RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
			@RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size,
			@RequestParam(defaultValue = "title") String sortBy, @RequestParam(defaultValue = "asc") String sortDir,
			Principal principal) {

		String email = principal.getName();
		System.out.println("Email: " + email);
		Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
		if (!userOpt.isPresent()) {
			throw new ErrorException("Không tìm thấy người dùng");
		}

		UserInformationEntity user = userOpt.get();
		Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
		Page<ConsultationScheduleDTO> schedules = consultationScheduleService.getConsultationsByConsultantWithFilters(
				user, title, statusPublic, statusConfirmed, mode, startDate, endDate, pageable);

		if (schedules.isEmpty()) {
			return ResponseEntity.status(404).body(DataResponse.<Page<ConsultationScheduleDTO>>builder().status("error")
					.message("Không tìm thấy lịch tư vấn.").build());
		}

		return ResponseEntity.ok(DataResponse.<Page<ConsultationScheduleDTO>>builder().status("success")
				.message("Lấy danh sách lịch tư vấn thành công.").data(schedules).build());
	}

	@PreAuthorize("hasRole('TUVANVIEN')")
	@PostMapping("/consultant/consultation-schedule/confirm")
	public ResponseEntity<DataResponse<String>> confirmConsultationSchedule(@RequestParam Integer scheduleId,
			@RequestBody ConsultationFeedbackRequest request, Principal principal) {

		String email = principal.getName();
		System.out.println("Email: " + email);
		Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
		if (!userOpt.isPresent()) {
			throw new ErrorException("Không tìm thấy người dùng");
		}

		UserInformationEntity consultant = userOpt.get();

		consultationScheduleService.confirmConsultationSchedule(scheduleId, request, consultant);

		ConsultationScheduleEntity schedule = consultationScheduleService.findConsulationScheduleById(scheduleId)
				.orElseThrow(() -> new ErrorException("Lịch tư vấn không tồn tại"));

		UserInformationEntity user = schedule.getUser();

		NotificationEntity notification = NotificationEntity.builder().senderId(consultant.getId())
				.receiverId(user.getId())
				.content(NotificationContent.CONFIRM_CONSULATION_SCHEDULE
						.formatMessage(user.getLastName() + " " + user.getFirstName()))
				.time(LocalDateTime.now())
				.notificationType(NotificationType.USER)
				.status(NotificationStatus.UNREAD)
				.build();

		notificationService.sendNotification(notification);
        System.out.println("Payload: " + notification);
        simpMessagingTemplate.convertAndSendToUser(String.valueOf(user.getId()), "/notification", notification);

		return ResponseEntity
				.ok(DataResponse.<String>builder().status("success").message("Lịch tư vấn đã được xác nhận.").build());
	}
	
	@PreAuthorize("hasRole('TUVANVIEN')")
	@PutMapping(value="/consultant/consultation-schedule/update", consumes = { "multipart/form-data" })
	public DataResponse<ManageConsultantScheduleDTO> updateConsultationScheduleForConsultant(
	        @RequestParam("scheduleId") Integer scheduleId,
	        @RequestParam("title") String title,
	        @RequestParam("content") String content,
	        @RequestParam("consultationDate") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate consultationDate,
	        @RequestParam("consultationTime") String consultationTime,
	        @RequestParam("location") String location,
	        @RequestParam("link") String link,
	        @RequestParam("mode") Boolean mode,
	        @RequestParam("statusPublic") Boolean statusPublic,
	        @RequestParam("statusConfirmed") Boolean statusConfirmed,
	        Principal principal) {

	    String email = principal.getName();
	    Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
	    if (!userOpt.isPresent()) {
	        throw new ErrorException("Không tìm thấy người dùng");
	    }

	    UserInformationEntity user = userOpt.get();

	    ConsultationScheduleEntity schedule = consultationScheduleRepository.findById(scheduleId)
	            .orElseThrow(() -> new ErrorException("Lịch tư vấn không tồn tại"));

	    if (schedule.getStatusConfirmed() != null && schedule.getStatusConfirmed()) {
	        throw new ErrorException("Bạn không thể cập nhật lịch tư vấn này vì nó đã được xác nhận.");
	    }
	    
	    if (!schedule.getConsultant().equals(user) || !schedule.getDepartment().getId().equals(user.getAccount().getDepartment().getId())) {
	        throw new ErrorException("Bạn không có quyền cập nhật lịch tư vấn này");
	    }

	    UpdateConsultationScheduleRequest scheduleRequest = UpdateConsultationScheduleRequest.builder()
	            .title(title)
	            .content(content)
	            .consultationDate(consultationDate)
	            .consultationTime(consultationTime)
	            .location(location)
	            .link(link)
	            .mode(mode)
	            .statusPublic(statusPublic)
	            .statusConfirmed(statusConfirmed)
	            .build();

	    ManageConsultantScheduleDTO updatedScheduleDTO = consultationScheduleService.updateConsultationSchedule(scheduleId, user.getAccount().getDepartment().getId(), scheduleRequest);

	    return DataResponse.<ManageConsultantScheduleDTO>builder()
	            .status("success")
	            .message("Cập nhật lịch tư vấn thành công.")
	            .data(updatedScheduleDTO)
	            .build();
	}

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	@PreAuthorize("hasRole('TRUONGBANTUVAN')")
	@GetMapping("/advisor/consultation-schedule/list")
	public ResponseEntity<DataResponse<Page<ConsultationScheduleDTO>>> getConsultationSchedulesByDepartment(
	        @RequestParam(required = false) String title, 
	        @RequestParam(required = false) Boolean statusPublic,
	        @RequestParam(required = false) Boolean statusConfirmed, 
	        @RequestParam(required = false) Boolean mode,
	        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
	        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
	        @RequestParam(defaultValue = "0") int page, 
	        @RequestParam(defaultValue = "10") int size,
	        @RequestParam(defaultValue = "title") String sortBy, 
	        @RequestParam(defaultValue = "asc") String sortDir,
	        Principal principal) {

	    String email = principal.getName();
	    System.out.println("Email: " + email);
	    Optional<UserInformationEntity> managerOpt = userRepository.findUserInfoByEmail(email);
	    if (!managerOpt.isPresent()) {
	        throw new ErrorException("Không tìm thấy người dùng");
	    }

	    UserInformationEntity manager = managerOpt.get();
	    Integer departmentId = manager.getAccount().getDepartment().getId();

	    Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
	    Page<ConsultationScheduleDTO> schedules = consultationScheduleService.getConsultationsByDepartmentWithFilters(
	            departmentId, title, statusPublic, statusConfirmed, mode, startDate, endDate, pageable);

	    if (schedules.isEmpty()) {
	        return ResponseEntity.status(404).body(DataResponse.<Page<ConsultationScheduleDTO>>builder().status("error")
	                .message("Không tìm thấy lịch tư vấn trong phòng ban.").build());
	    }

	    return ResponseEntity.ok(DataResponse.<Page<ConsultationScheduleDTO>>builder().status("success")
	            .message("Lấy danh sách lịch tư vấn trong phòng ban thành công.").data(schedules).build());
	}

	@PreAuthorize("hasRole('TRUONGBANTUVAN')")
	@PostMapping("/advisor/consultation-schedule/create")
	public ResponseEntity<DataResponse<ManageConsultantScheduleDTO>> createConsultationSchedule(
	        @RequestBody ManageCreateConsultantScheduleRequest request, 
	        Principal principal) {

	    String email = principal.getName();
	    System.out.println("Email: " + email);
	    Optional<UserInformationEntity> managerOpt = userRepository.findUserInfoByEmail(email);
	    if (!managerOpt.isPresent()) {
	        throw new ErrorException("Không tìm thấy người dùng");
	    }

	    UserInformationEntity manager = managerOpt.get();
	    Integer departmentId = manager.getAccount().getDepartment().getId();

	    ManageConsultantScheduleDTO consultationSchedule = consultationScheduleService.createConsultationSchedule(request, departmentId);

	    return ResponseEntity.ok(DataResponse.<ManageConsultantScheduleDTO>builder()
	            .status("success")
	            .message("Tạo buổi tư vấn thành công.")
	            .data(consultationSchedule)
	            .build());
	}
	
	@PreAuthorize("hasRole('TRUONGBANTUVAN')")
	@GetMapping("/advisor/consultation-schedule-owner/list")
	public ResponseEntity<DataResponse<Page<ManageConsultantScheduleDTO>>> getConsultationsByDepartmentOwner(
	        @RequestParam(required = false) String title, 
	        @RequestParam(required = false) Boolean statusPublic,
	        @RequestParam(required = false) Boolean statusConfirmed, 
	        @RequestParam(required = false) Boolean mode,
	        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
	        @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
	        @RequestParam(defaultValue = "0") int page, 
	        @RequestParam(defaultValue = "10") int size,
	        @RequestParam(defaultValue = "title") String sortBy, 
	        @RequestParam(defaultValue = "asc") String sortDir,
	        Principal principal) {

	    String email = principal.getName();
	    System.out.println("Email: " + email);
	    Optional<UserInformationEntity> managerOpt = userRepository.findUserInfoByEmail(email);
	    if (!managerOpt.isPresent()) {
	        throw new ErrorException("Không tìm thấy người dùng");
	    }

	    UserInformationEntity manager = managerOpt.get();
	    Integer departmentId = manager.getAccount().getDepartment().getId();

	    Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
	    Page<ManageConsultantScheduleDTO> schedules = consultationScheduleService.getConsultationsByDepartmentOwnerWithFilters(
	            departmentId, title, statusPublic, statusConfirmed, mode, startDate, endDate, pageable);

	    if (schedules.isEmpty()) {
	        return ResponseEntity.status(404).body(DataResponse.<Page<ManageConsultantScheduleDTO>>builder().status("error")
	                .message("Không tìm thấy lịch tư vấn trong phòng ban.").build());
	    }

	    return ResponseEntity.ok(DataResponse.<Page<ManageConsultantScheduleDTO>>builder().status("success")
	            .message("Lấy danh sách lịch tư vấn trong phòng ban thành công.").data(schedules).build());
	}
	
	@PreAuthorize("hasRole('TRUONGBANTUVAN')")
	@PutMapping(value="/advisor/consultation-schedule/update", consumes = { "multipart/form-data" })
	public DataResponse<ManageConsultantScheduleDTO> updateConsultationSchedule(
	        @RequestParam("scheduleId") Integer scheduleId,
	        @RequestParam("title") String title,
	        @RequestParam("content") String content,
	        @RequestParam("consultationDate") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate consultationDate,
	        @RequestParam("consultationTime") String consultationTime,
	        @RequestParam("location") String location,
	        @RequestParam("link") String link,
	        @RequestParam("mode") Boolean mode,
	        @RequestParam("statusPublic") Boolean statusPublic,
	        @RequestParam("statusConfirmed") Boolean statusConfirmed,
	        Principal principal) {

	    String email = principal.getName();
	    Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
	    if (!userOpt.isPresent()) {
	        throw new ErrorException("Không tìm thấy người dùng");
	    }

	    UserInformationEntity user = userOpt.get();
	    Integer departmentId = user.getAccount().getDepartment().getId();

	    UpdateConsultationScheduleRequest scheduleRequest = UpdateConsultationScheduleRequest.builder()
	            .title(title)
	            .content(content)
	            .consultationDate(consultationDate)
	            .consultationTime(consultationTime)
	            .location(location)
	            .link(link)
	            .mode(mode)
	            .statusPublic(statusPublic)
	            .statusConfirmed(statusConfirmed)
	            .build();

	    ManageConsultantScheduleDTO updatedScheduleDTO = consultationScheduleService.updateConsultationSchedule(scheduleId, departmentId, scheduleRequest);

	    return DataResponse.<ManageConsultantScheduleDTO>builder()
	            .status("success")
	            .message("Cập nhật lịch tư vấn thành công.")
	            .data(updatedScheduleDTO)
	            .build();
	}
	
	@PreAuthorize("hasRole('TRUONGBANTUVAN')")
	@DeleteMapping("/advisor/consultation-schedule/delete")
	public ResponseEntity<DataResponse<Void>> deleteConsultationSchedule(@RequestParam Integer scheduleId, Principal principal) {
	    String email = principal.getName();
	    Optional<UserInformationEntity> managerOpt = userRepository.findUserInfoByEmail(email);
	    if (!managerOpt.isPresent()) {
	        throw new ErrorException("Không tìm thấy người dùng");
	    }

	    UserInformationEntity manager = managerOpt.get();
	    Integer departmentId = manager.getAccount().getDepartment().getId();

	    consultationScheduleService.deleteConsultationSchedule(scheduleId, departmentId);
	    return ResponseEntity.ok(DataResponse.<Void>builder()
	            .status("success")
	            .message("Xóa lịch tư vấn thành công.")
	            .build());
	}
}
