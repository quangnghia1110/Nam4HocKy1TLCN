package studentConsulting.controller.user;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
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
import studentConsulting.model.payload.dto.notification.NotificationResponseDTO;
import studentConsulting.model.payload.request.consultant.ConsultationScheduleRegistrationRequest;
import studentConsulting.model.payload.request.consultant.CreateScheduleConsultationRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.consultation_schedule.ConsultationScheduleRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.common.ICommonNotificationService;
import studentConsulting.service.interfaces.common.ICommonUserService;
import studentConsulting.service.interfaces.user.IUserConsultationScheduleService;

import java.security.Principal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class UserConsultationScheduleController {

    @Autowired
    private IUserConsultationScheduleService consultationScheduleService;

    @Autowired
    private ICommonUserService userService;

    @Autowired
    private ICommonNotificationService notificationService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

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
}
