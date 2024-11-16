package studentConsulting.controller.actor;

import com.itextpdf.text.DocumentException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.AdvisorSummaryDTO;
import studentConsulting.model.payload.dto.actor.RatingDTO;
import studentConsulting.model.payload.request.CreateRatingRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.interfaces.actor.IRatingService;
import studentConsulting.service.interfaces.common.IGuestService;
import studentConsulting.service.interfaces.common.INotificationService;
import studentConsulting.service.interfaces.common.IUserService;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.security.Principal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class RatingController {
    @Autowired
    private IRatingService ratingService;

    @Autowired
    private IUserService userService;
    @Autowired
    private UserRepository userRepository;

    @Autowired
    private IGuestService consultantService;

    @Autowired
    private INotificationService notificationService;

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @PostMapping("/user/rating/create")
    public ResponseEntity<DataResponse<RatingDTO>> create(@RequestBody CreateRatingRequest request, Principal principal) {
        String email = principal.getName();
        System.out.println("Email: " + email);

        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        RatingDTO createRating = ratingService.createRating(request, user);

        Optional<UserInformationEntity> consultantOpt = userRepository.findById(request.getConsultantId());
        consultantOpt.ifPresent(consultant -> {

            notificationService.sendUserNotification(
                    user.getId(),
                    consultant.getId(),
                    NotificationContent.NEW_RATING_RECEIVED.formatMessage(user.getLastName() + " " + user.getFirstName()),
                    NotificationType.TUVANVIEN
            );

            Optional<UserInformationEntity> advisorOpt = userRepository.findByRoleAndDepartment(
                    SecurityConstants.Role.TRUONGBANTUVAN, consultant.getAccount().getDepartment().getId());

            advisorOpt.ifPresent(headOfDepartment -> {
                notificationService.sendUserNotification(
                        user.getId(),
                        headOfDepartment.getId(),
                        NotificationContent.NEW_RATING_RECEIVED.formatMessage(user.getLastName() + " " + user.getFirstName()),
                        NotificationType.TRUONGBANTUVAN
                );
            });
        });

        return ResponseEntity.ok(DataResponse.<RatingDTO>builder()
                .status("success")
                .message("Mẫu đánh giá đã được tạo thành công.")
                .data(createRating)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or "
            + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or "
            + SecurityConstants.PreAuthorize.ADMIN + " or "
            + SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/rating/list")
    public ResponseEntity<DataResponse<Page<RatingDTO>>> getListRatingByRole(
            @RequestParam(required = false) Integer departmentId,
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

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        boolean isAdvisor = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TRUONGBANTUVAN);
        boolean isConsultant = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TUVANVIEN);
        Integer depId = user.getAccount().getDepartment().getId();

        Page<RatingDTO> ratings = ratingService.getListRatingByRole(
                email, departmentId, consultantName, startDate, endDate,
                page, size, sortBy, sortDir, isAdmin, isAdvisor, isConsultant, depId);

        if (ratings.isEmpty()) {
            return ResponseEntity.ok(DataResponse.<Page<RatingDTO>>builder()
                    .status("error")
                    .message("Không có đánh giá nào được tìm thấy.")
                    .data(Page.empty())
                    .build());
        }

        return ResponseEntity.ok(DataResponse.<Page<RatingDTO>>builder()
                .status("success")
                .message("Lấy danh sách đánh giá thành công")
                .data(ratings)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or "
            + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or "
            + SecurityConstants.PreAuthorize.ADMIN + " or "
            + SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/rating/detail")
    public ResponseEntity<DataResponse<RatingDTO>> getDetailRatingByRole(
            @RequestParam("id") Integer ratingId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        boolean isAdvisor = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TRUONGBANTUVAN);
        boolean isConsultant = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TUVANVIEN);
        Integer depId = user.getAccount().getDepartment().getId();

        RatingDTO ratingDTO = ratingService.getDetailRatingByRole(
                ratingId, email, depId, isAdmin, isAdvisor, isConsultant);

        return ResponseEntity.ok(DataResponse.<RatingDTO>builder()
                .status("success")
                .message("Lấy chi tiết đánh giá thành công.")
                .data(ratingDTO)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @GetMapping("/list-consultant-rating-by-department")
    public ResponseEntity<DataResponse<RatingDTO>> getRatingByConsultant(@RequestParam Integer consultantId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        RatingDTO rating = ratingService.getRatingByConsultantId(consultantId, user.getId());

        return ResponseEntity.ok(
                DataResponse.<RatingDTO>builder().status("success").message("Đánh giá của tư vấn viên").data(rating).build()
        );
    }
}
