package studentConsulting.controller.advisor;

import com.lowagie.text.DocumentException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.rating.RatingDTO;
import studentConsulting.model.payload.dto.user.AdvisorSummaryDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorRatingService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonUserService;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.security.Principal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdvisorRatingController {

    @Autowired
    private IAdvisorRatingService ratingService;

    @Autowired
    private ICommonUserService userService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommonExcelService commonExcelService;

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor-admin/rating/list")
    public ResponseEntity<DataResponse<Page<RatingDTO>>> getRatingsByDepartment(
            @RequestParam(required = false) String consultantName,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "submittedAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = userOpt.get();
        boolean isAdmin = manager.getAccount().getRole().getName().equals("ROLE_ADMIN");
        Integer departmentId = isAdmin ? null : manager.getAccount().getDepartment().getId();

        Page<RatingDTO> ratings = ratingService.getRatingsByDepartment(departmentId, consultantName, startDate, endDate, page, size, sortBy, sortDir);
        if (ratings.isEmpty()) {
            throw new ErrorException("Không tìm thấy đánh giá nào.");
        }
        return ResponseEntity.ok(DataResponse.<Page<RatingDTO>>builder()
                .status("success")
                .message("Lấy danh sách đánh giá thành công.")
                .data(ratings)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor-admin/rating/detail")
    public ResponseEntity<DataResponse<RatingDTO>> getRatingByIdAndDepartment(@RequestParam("id") Integer ratingId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = userOpt.get();
        boolean isAdmin = manager.getAccount().getRole().getName().equals("ROLE_ADMIN");
        Integer departmentId = isAdmin ? null : manager.getAccount().getDepartment().getId();

        RatingDTO ratingDTO = ratingService.getRatingByIdAndDepartment(ratingId, departmentId);
        if (ratingDTO == null) {
            throw new ErrorException("Không tìm thấy đánh giá");
        }

        return ResponseEntity.ok(DataResponse.<RatingDTO>builder()
                .status("success")
                .message("Lấy chi tiết đánh giá thành công.")
                .data(ratingDTO)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/rating/summary/pdf")
    public void exportAdvisorSummaryToPdf(HttpServletResponse response, Principal principal) throws DocumentException {
        try {
            String email = principal.getName();
            Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
            if (!userOpt.isPresent()) {
                throw new ErrorException("Không tìm thấy người dùng");
            }

            UserInformationEntity manager = userOpt.get();
            boolean isAdmin = manager.getAccount().getRole().getName().equals("ROLE_ADMIN");
            Integer departmentId = isAdmin ? null : manager.getAccount().getDepartment().getId();

            List<AdvisorSummaryDTO> summaries = ratingService.getAdvisorSummariesByDepartment(departmentId);
            if (summaries.isEmpty()) {
                throw new ErrorException("Không có đánh giá nào được tìm thấy.");
            }

            ratingService.generateAdvisorSummaryPdf(summaries, response);

        } catch (DocumentException | IOException e) {
            e.printStackTrace();
            throw new ErrorException("Có lỗi xảy ra khi tạo PDF.");
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/rating/summary/excel")
    public void exportAdvisorSummaryToExcel(HttpServletResponse response, Principal principal) {
        try {
            String email = principal.getName();
            Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
            if (!userOpt.isPresent()) {
                throw new ErrorException("Không tìm thấy người dùng");
            }

            UserInformationEntity manager = userOpt.get();
            boolean isAdmin = manager.getAccount().getRole().getName().equals("ROLE_ADMIN");
            Integer departmentId = isAdmin ? null : manager.getAccount().getDepartment().getId();

            List<AdvisorSummaryDTO> summaries = ratingService.getAdvisorSummariesByDepartment(departmentId);
            if (summaries.isEmpty()) {
                throw new ErrorException("Không có đánh giá nào được tìm thấy.");
            }

            ratingService.generateAdvisorSummaryExcel(summaries, response);

        } catch (IOException e) {
            e.printStackTrace();
            throw new ErrorException("Có lỗi xảy ra khi tạo file Excel.");
        }
    }
}
