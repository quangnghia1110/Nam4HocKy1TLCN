package studentConsulting.controller.consultant;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.statistic.ConsultantStatisticsDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.consultant.IConsultantStatisticsService;

import java.security.Principal;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class ConsultantStatisticsController {

    @Autowired
    private IConsultantStatisticsService statisticsService;

    @Autowired
    private UserRepository userRepository;

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/consultant/statistics")
    public ResponseEntity<DataResponse<ConsultantStatisticsDTO>> getConsultantStatistics(Principal principal) {
        String email = principal.getName();
        System.out.println("Email: " + email);

        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        ConsultantStatisticsDTO data = statisticsService.getConsultantStatistics(user.getId());

        if (data == null) {
            throw new ErrorException("Không tìm thấy thống kê tư vấn viên");
        }

        return ResponseEntity.ok(DataResponse.<ConsultantStatisticsDTO>builder()
                .status("success")
                .message("Lấy thống kê tư vấn viên thành công")
                .data(data)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/consultant/statistics/deleted-questions")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getDeletedQuestionsByYear(
            Principal principal,
            @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        List<Map<String, Object>> data = statisticsService.getDeletedQuestionsByYear(user.getId(), year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê câu hỏi đã bị xóa thành công")
                .data(data)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/consultant/statistics/answers-given")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getAnswersGivenByYear(
            Principal principal,
            @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        List<Map<String, Object>> data = statisticsService.getAnswersGivenByYear(user.getId(), year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê câu trả lời đã đưa ra thành công")
                .data(data)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/consultant/statistics/answer-approvals")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getAnswerApprovalByYear(
            Principal principal,
            @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        List<Map<String, Object>> data = statisticsService.getAnswerApprovalByYear(user.getId(), year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê câu trả lời cần duyệt thành công")
                .data(data)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/consultant/statistics/consultation-schedules")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getConsultationSchedulesConsultantByYear(
            Principal principal,
            @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        List<Map<String, Object>> data = statisticsService.getConsultationSchedulesConsultantByYear(user.getId(), year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê lịch tư vấn thành công")
                .data(data)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/consultant/statistics/approved-posts")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getApprovedPostsByYear(
            Principal principal,
            @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        List<Map<String, Object>> data = statisticsService.getApprovedPostsByYear(user.getId(), year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê bài đăng đã duyệt thành công")
                .data(data)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/consultant/statistics/conversations")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getConversationsConsultantByYear(
            Principal principal,
            @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        List<Map<String, Object>> data = statisticsService.getConversationsConsultantByYear(user.getId(), year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê cuộc hội thoại thành công")
                .data(data)
                .build());
    }

}
