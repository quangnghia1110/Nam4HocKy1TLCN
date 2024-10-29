package studentConsulting.controller.actor;

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
import studentConsulting.model.entity.consultation_schedule.ConsultationScheduleEntity;
import studentConsulting.model.entity.notification.NotificationEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleRegistrationDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleRegistrationMemberDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.dto.notification.NotificationResponseDTO;
import studentConsulting.model.payload.request.consultant.ConsultationScheduleRegistrationRequest;
import studentConsulting.model.payload.request.consultant.CreateScheduleConsultationRequest;
import studentConsulting.model.payload.request.consultant.ManageCreateConsultantScheduleRequest;
import studentConsulting.model.payload.request.consultant.UpdateConsultationScheduleRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.consultation_schedule.ConsultationScheduleRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.actor.IConsultationScheduleService;
import studentConsulting.service.interfaces.common.INotificationService;
import studentConsulting.service.interfaces.common.IUserService;

import java.security.Principal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Optional;

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

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
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

        NotificationEntity notification = NotificationEntity.builder()
                .senderId(user.getId())
                .receiverId(consultant.getId())
                .content(NotificationContent.NEW_CONSULATION_SCHEDULE.formatMessage(user.getLastName() + " " + user.getFirstName()))
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

        simpMessagingTemplate.convertAndSendToUser(String.valueOf(consultant.getId()), "/private", responseDTO);

        return ResponseEntity.ok(DataResponse.<ConsultationScheduleDTO>builder().status("success")
                .message("Lịch tư vấn đã được tạo thành công.").data(createdSchedule).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @PostMapping(value = "/consultant/consultation-schedule/confirm", consumes = {"application/json"})
    public DataResponse<ManageConsultantScheduleDTO> confirmConsultationScheduleForConsultant(
            @RequestParam("scheduleId") Integer scheduleId,
            @RequestBody UpdateConsultationScheduleRequest scheduleRequest,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        if (!user.getAccount().getRoleConsultant().getName().equals("GIANGVIEN")) {
            throw new ErrorException("Chỉ có giảng viên mới có thể xác nhận lịch tư vấn.");
        }

        ConsultationScheduleEntity schedule = consultationScheduleRepository.findById(scheduleId)
                .orElseThrow(() -> new ErrorException("Lịch tư vấn không tồn tại"));

        if (schedule.getStatusConfirmed() != null && schedule.getStatusConfirmed()) {
            throw new ErrorException("Bạn không thể cập nhật lịch tư vấn này vì nó đã được xác nhận.");
        }

        if (!schedule.getConsultant().equals(user) || !schedule.getDepartment().getId().equals(user.getAccount().getDepartment().getId())) {
            throw new ErrorException("Bạn không có quyền cập nhật lịch tư vấn này");
        }

        ManageConsultantScheduleDTO updatedScheduleDTO = consultationScheduleService.confirmConsultationSchedule(
                scheduleId, user.getAccount().getDepartment().getId(), scheduleRequest);

        return DataResponse.<ManageConsultantScheduleDTO>builder()
                .status("success")
                .message("Xác nhận lịch tư vấn thành công.")
                .data(updatedScheduleDTO)
                .build();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/consultation-schedule/create")
    public ResponseEntity<DataResponse<ConsultationScheduleDTO>> createConsultationSchedule(
            @RequestBody ManageCreateConsultantScheduleRequest request,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        Integer departmentId = isAdmin ? null : user.getAccount().getDepartment().getId();
        Integer userId = user.getId();

        ConsultationScheduleDTO consultationSchedule = consultationScheduleService.createConsultationSchedule(request, departmentId, userId);

        return ResponseEntity.ok(DataResponse.<ConsultationScheduleDTO>builder()
                .status("success")
                .message("Tạo buổi tư vấn thành công.")
                .data(consultationSchedule)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/consultation-schedule/list")
    public ResponseEntity<DataResponse<Page<ConsultationScheduleDTO>>> getConsultationSchedulesByRole(
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
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ConsultationScheduleDTO> schedules = consultationScheduleService.getConsultationScheduleByRole(user, title, statusPublic, statusConfirmed, mode, startDate, endDate, pageable);

        if (schedules.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<ConsultationScheduleDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy lịch tư vấn.")
                            .build());
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ConsultationScheduleDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách lịch tư vấn thành công.")
                        .data(schedules)
                        .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping(value = "/consultation-schedule/update", consumes = {"multipart/form-data"})
    public DataResponse<ConsultationScheduleDTO> updateConsultationSchedule(
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
        String role = user.getAccount().getRole().getName();
        boolean isAdmin = SecurityConstants.Role.ADMIN.equals(role);
        Integer departmentId = isAdmin ? null : user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;

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

        ConsultationScheduleDTO updatedScheduleDTO = consultationScheduleService.updateConsultationSchedule(scheduleId, departmentId, isAdmin, scheduleRequest, role, user.getId());

        return DataResponse.<ConsultationScheduleDTO>builder()
                .status("success")
                .message("Cập nhật lịch tư vấn thành công.")
                .data(updatedScheduleDTO)
                .build();
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/consultation-schedule/delete")
    public ResponseEntity<DataResponse<Void>> deleteConsultationSchedule(
            @RequestParam Integer scheduleId, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        String role = user.getAccount().getRole().getName();
        Integer departmentId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;

        consultationScheduleService.deleteConsultationSchedule(scheduleId, departmentId, user.getId(), role);

        return ResponseEntity.ok(DataResponse.<Void>builder()
                .status("success")
                .message("Xóa lịch tư vấn thành công.")
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/consultation-schedule/detail")
    public ResponseEntity<DataResponse<ConsultationScheduleDTO>> getConsultationScheduleDetail(
            @RequestParam("scheduleId") Integer scheduleId, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        String role = user.getAccount().getRole().getName();
        Integer departmentId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;
        Integer userId = user.getId();

        ConsultationScheduleDTO scheduleDTO = consultationScheduleService.getConsultationScheduleByRole(scheduleId, role, departmentId, userId);

        return ResponseEntity.ok(DataResponse.<ConsultationScheduleDTO>builder()
                .status("success")
                .message("Lấy chi tiết lịch tư vấn thành công.")
                .data(scheduleDTO)
                .build());
    }
    
    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @GetMapping("/user/consultation-schedule/list-join")
    public ResponseEntity<DataResponse<Page<ConsultationScheduleRegistrationDTO>>> getSchedulesJoinByUser(
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "registeredAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConsultationScheduleRegistrationDTO> schedules = consultationScheduleService.getSchedulesJoinByUser(
                user, startDate, endDate, pageable);

        if (schedules.isEmpty()) {
            return ResponseEntity.status(404).body(DataResponse.<Page<ConsultationScheduleRegistrationDTO>>builder()
                    .status("error").message("Không tìm thấy lịch tư vấn.").build());
        }

        return ResponseEntity.ok(DataResponse.<Page<ConsultationScheduleRegistrationDTO>>builder()
                .status("success").message("Lấy danh sách lịch tư vấn thành công.").data(schedules).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @PostMapping("/user/consultation-schedule/join")
    public ResponseEntity<DataResponse<ConsultationScheduleRegistrationDTO>> joinConsultationSchedule(
            @RequestBody ConsultationScheduleRegistrationRequest request, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);

        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();

        ConsultationScheduleEntity consultationSchedule = consultationScheduleRepository.findById(request.getConsultationScheduleId())
                .orElseThrow(() -> new ErrorException("Lịch tư vấn không tồn tại"));

        ConsultationScheduleRegistrationDTO registrationDTO = consultationScheduleService.registerForConsultation(request, user);


        return ResponseEntity.ok(DataResponse.<ConsultationScheduleRegistrationDTO>builder().status("success")
                .message("Đăng ký lịch tư vấn công khai thành công.").data(registrationDTO).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @PostMapping("/user/consultation-schedule/cancel")
    public ResponseEntity<DataResponse<Void>> cancelConsultationSchedule(
            @RequestParam Integer id, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        consultationScheduleService.cancelRegistrationForConsultation(id, user);

        return ResponseEntity.ok(DataResponse.<Void>builder()
                .status("success")
                .message("Hủy đăng ký lịch tư vấn thành công.")
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor-admin/consultation-schedule/list-member-join")
    public ResponseEntity<DataResponse<Page<ConsultationScheduleRegistrationMemberDTO>>> getMembersByConsultationSchedule(
            @RequestParam Integer consultationScheduleId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "registeredAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConsultationScheduleRegistrationMemberDTO> members;

        if (isAdmin) {
            members = consultationScheduleService.getMembersByConsultationSchedule(consultationScheduleId, startDate, endDate, pageable, null);
        } else {
            members = consultationScheduleService.getMembersByConsultationSchedule(consultationScheduleId, startDate, endDate, pageable, user.getId());
        }

        if (members.isEmpty()) {
            return ResponseEntity.status(404).body(DataResponse.<Page<ConsultationScheduleRegistrationMemberDTO>>builder()
                    .status("error").message("Không tìm thấy thành viên nào tham gia buổi tư vấn này.").build());
        }

        return ResponseEntity.ok(DataResponse.<Page<ConsultationScheduleRegistrationMemberDTO>>builder()
                .status("success").message("Lấy danh sách thành viên thành công.").data(members).build());
    }
}
