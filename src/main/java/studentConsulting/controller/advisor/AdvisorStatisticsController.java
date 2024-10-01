package studentConsulting.controller.advisor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.statistic.AdvisorStatisticsDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorStatisticsService;

import java.security.Principal;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdvisorStatisticsController {
    @Autowired
    private IAdvisorStatisticsService statisticsService;

    @Autowired
    private UserRepository userRepository;

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/statistics")
    public ResponseEntity<DataResponse<AdvisorStatisticsDTO>> getManagerStatistics(Principal principal) {
        String email = principal.getName();
        System.out.println("Email: " + email);

        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        AdvisorStatisticsDTO data = statisticsService.getAdvisorStatistics(user.getId());

        if (data == null) {
            throw new ErrorException("Không tìm thấy thống kê của trưởng ban");
        }

        return ResponseEntity.ok(DataResponse.<AdvisorStatisticsDTO>builder()
                .status("success")
                .message("Lấy thống kê của trưởng ban thành công")
                .data(data)
                .build());
    }

}
