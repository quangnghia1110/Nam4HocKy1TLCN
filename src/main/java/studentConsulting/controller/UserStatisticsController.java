package studentConsulting.controller;

import java.security.Principal;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.UserStatisticsDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.implement.UserStatisticsService;

@RestController
@RequestMapping("${base.url}")
public class UserStatisticsController {

    @Autowired
    private UserStatisticsService userStatisticsService;

    @Autowired
    private UserRepository userRepository;

    @PreAuthorize("hasRole('USER')")
    @GetMapping("/user/statistics")
    public ResponseEntity<DataResponse<UserStatisticsDTO>> getUserStatistics(Principal principal) {
        String email = principal.getName();
        System.out.println("Email: " + email);

        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        UserStatisticsDTO statistics = userStatisticsService.getUserStatistics(user.getId());
        return ResponseEntity.ok(DataResponse.<UserStatisticsDTO>builder()
                .status("success")
                .message("Lấy thống kê thành công")
                .data(statistics)
                .build());
    }

    @PreAuthorize("hasRole('USER')")
    @GetMapping("/user/statistics/questions-status/timeframe")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getQuestionsByStatusAndTimeFrame(
            Principal principal,
            @RequestParam String statusKey,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        List<Map<String, Object>> statistics = userStatisticsService.getQuestionsByStatusAndTimeFrame(
                user.getId(), statusKey, startDate, endDate);

        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy danh sách câu hỏi thành công.")
                .data(statistics)
                .build());
    }

    @PreAuthorize("hasRole('USER')")
    @GetMapping("/user/statistics/ratings/timeframe")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getRatingsByTimeFrame(
            Principal principal,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        List<Map<String, Object>> ratings = userStatisticsService.getRatingsByTimeFrame(
                user.getId(), startDate, endDate);

        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê ratings thành công.")
                .data(ratings)
                .build());
    }
    
    @PreAuthorize("hasRole('USER')")
    @GetMapping("/user/statistics/consultationSchedule/timeframe")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getConsultationSchedulesByTimeFrame(
            Principal principal,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        List<Map<String, Object>> appointments = userStatisticsService.getConsultationSchedulesByTimeFrame(
                user.getId(), startDate, endDate);

        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy danh sách lịch hẹn thành công.")
                .data(appointments)
                .build());
    }

    @PreAuthorize("hasRole('USER')")
    @GetMapping("/user/statistics/conversations/timeframe")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getConversationsByTimeFrame(
            Principal principal,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        List<Map<String, Object>> conversations = userStatisticsService.getConversationsByTimeFrame(
                user.getId(), startDate, endDate);

        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy danh sách cuộc hội thoại thành công.")
                .data(conversations)
                .build());
    }
    
    @PreAuthorize("hasRole('USER')")
    @GetMapping("/user/statistics/questions-department-field/timeframe")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getQuestionsByDepartmentAndField(
            Principal principal,
            @RequestParam Integer departmentId,
            @RequestParam Integer fieldId,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        List<Map<String, Object>> statistics = userStatisticsService.getQuestionsByDepartmentAndField(
                user.getId(), departmentId, fieldId, startDate, endDate);

        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy danh sách câu hỏi theo phòng ban và lĩnh vực thành công.")
                .data(statistics)
                .build());
    }

}
