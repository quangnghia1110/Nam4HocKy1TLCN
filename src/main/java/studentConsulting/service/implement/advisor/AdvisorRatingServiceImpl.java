package studentConsulting.service.implement.advisor;

import com.lowagie.text.DocumentException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.rating.RatingEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.department_field.DepartmentDTO;
import studentConsulting.model.payload.dto.rating.RatingDTO;
import studentConsulting.model.payload.dto.user.AdvisorSummaryDTO;
import studentConsulting.model.payload.dto.user.UserRatingDTO;
import studentConsulting.repository.rating.RatingRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorRatingService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonPdfService;
import studentConsulting.specification.rating.RatingSpecification;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class AdvisorRatingServiceImpl implements IAdvisorRatingService {

    @Autowired
    private RatingRepository ratingRepository;

    @Autowired
    private ICommonPdfService pdfService;

    @Autowired
    private ICommonExcelService excelService;

    @Override
    public Page<RatingDTO> getRatingsByDepartment(Integer departmentId, String consultantName, LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir) {
        Specification<RatingEntity> spec = Specification.where(RatingSpecification.hasDepartment(departmentId));

        if (consultantName != null && !consultantName.isEmpty()) {
            spec = spec.and(RatingSpecification.hasConsultantName(consultantName));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(RatingSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(RatingSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(RatingSpecification.hasDateBefore(endDate));
        }

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<RatingEntity> ratingEntities = ratingRepository.findAll(spec, pageable);

        return ratingEntities.map(this::mapToDTO);
    }

    private RatingDTO mapToDTO(RatingEntity rating) {
        return RatingDTO.builder()
                .id(rating.getId())
                .department(rating.getDepartment() != null
                        ? new DepartmentDTO(rating.getDepartment().getId(), rating.getDepartment().getName())
                        : null)
                .user(RatingDTO.UserDTO.builder()
                        .id(rating.getUser().getId())
                        .name(rating.getUser().getLastName() + " " + rating.getUser().getFirstName())
                        .build())
                .consultant(RatingDTO.UserDTO.builder()
                        .id(rating.getConsultant().getId())
                        .name(rating.getConsultant().getLastName() + " " + rating.getConsultant().getFirstName())
                        .build())
                .generalSatisfaction(rating.getGeneralSatisfaction())
                .generalComment(rating.getGeneralComment())
                .expertiseKnowledge(rating.getExpertiseKnowledge())
                .expertiseComment(rating.getExpertiseComment())
                .attitude(rating.getAttitude())
                .attitudeComment(rating.getAttitudeComment())
                .responseSpeed(rating.getResponseSpeed())
                .responseSpeedComment(rating.getResponseSpeedComment())
                .understanding(rating.getUnderstanding())
                .understandingComment(rating.getUnderstandingComment())
                .submittedAt(rating.getSubmittedAt())
                .build();
    }

    @Override
    public RatingDTO getRatingByIdAndDepartment(Integer ratingId, Integer departmentId) {
        Optional<RatingEntity> ratingOpt = ratingRepository.findByIdAndDepartmentId(ratingId, departmentId);
        if (!ratingOpt.isPresent()) {
            throw new ErrorException("Đánh giá không tồn tại");
        }
        RatingEntity rating = ratingOpt.get();
        return mapToDTO(rating);
    }

    @Override
    public List<AdvisorSummaryDTO> getAdvisorSummariesByDepartment(Integer departmentId) {
        List<RatingEntity> ratings = ratingRepository.findAllByDepartmentId(departmentId);
        if (ratings.isEmpty()) {
            throw new ErrorException("Không có đánh giá nào cho phòng ban này.");
        }

        Map<UserInformationEntity, List<RatingEntity>> ratingsGroupedByConsultant = ratings.stream()
                .collect(Collectors.groupingBy(RatingEntity::getConsultant));

        return ratingsGroupedByConsultant.entrySet().stream()
                .map(entry -> {
                    UserRatingDTO consultantDTO = convertToUserDTO(entry.getKey());
                    List<RatingEntity> consultantRatings = entry.getValue();

                    long numberOfRatings = consultantRatings.size();

                    double avgGeneralSatisfaction = consultantRatings.stream()
                            .mapToInt(RatingEntity::getGeneralSatisfaction)
                            .average()
                            .orElse(0.0);

                    double avgExpertiseKnowledge = consultantRatings.stream()
                            .mapToInt(RatingEntity::getExpertiseKnowledge)
                            .average()
                            .orElse(0.0);

                    double avgAttitude = consultantRatings.stream()
                            .mapToInt(RatingEntity::getAttitude)
                            .average()
                            .orElse(0.0);

                    double avgResponseSpeed = consultantRatings.stream()
                            .mapToInt(RatingEntity::getResponseSpeed)
                            .average()
                            .orElse(0.0);

                    double avgUnderstanding = consultantRatings.stream()
                            .mapToInt(RatingEntity::getUnderstanding)
                            .average()
                            .orElse(0.0);

                    double avgOverallScore = (avgGeneralSatisfaction + avgExpertiseKnowledge + avgAttitude + avgResponseSpeed + avgUnderstanding) / 5.0;

                    return AdvisorSummaryDTO.builder()
                            .consultantId(consultantDTO.getId())
                            .consultantName(consultantDTO.getLastName() + " " + consultantDTO.getFirstName())
                            .consultantEmail(consultantDTO.getEmail())
                            .numberOfRatings(numberOfRatings)
                            .avgGeneralSatisfaction(avgGeneralSatisfaction)
                            .avgExpertiseKnowledge(avgExpertiseKnowledge)
                            .avgAttitude(avgAttitude)
                            .avgResponseSpeed(avgResponseSpeed)
                            .avgUnderstanding(avgUnderstanding)
                            .avgOverallScore(avgOverallScore)
                            .build();
                })
                .collect(Collectors.toList());
    }

    private UserRatingDTO convertToUserDTO(UserInformationEntity entity) {
        return UserRatingDTO.builder()
                .id(entity.getId())
                .firstName(entity.getFirstName())
                .lastName(entity.getLastName())
                .email(entity.getAccount() != null ? entity.getAccount().getEmail() : null)
                .build();
    }


    public void generateAdvisorSummaryPdf(List<AdvisorSummaryDTO> summaries, HttpServletResponse response) throws DocumentException, IOException {
        Map<String, String> placeholders = new HashMap<>();
        placeholders.put("{{logo_url}}", "https://lh4.googleusercontent.com/proxy/L8S29oTCuu_R0eqZH-cnWHvW0nrEa-ZHILpFb2btfiQRbL5vzZ01TiT8WyaG2B8mMguiuV_WYnpHDzCjzZrUNTI83UNg6tL1K4I1uViJ9-tl_CJeZoIwmY5rYA");
        placeholders.put("{{data_rows}}", buildDataRows(summaries));

        pdfService.generatePdfFromTemplate("/templates/advisor_summary_template.html", placeholders, "advisor_summary_" + pdfService.currentDate(), response);
    }

    private String buildDataRows(List<AdvisorSummaryDTO> summaries) {
        StringBuilder dataRows = new StringBuilder();

        for (AdvisorSummaryDTO summary : summaries) {
            dataRows.append("<tr>")
                    .append("<td>").append(summary.getConsultantName()).append("</td>")
                    .append("<td>").append(summary.getConsultantEmail()).append("</td>")
                    .append("<td>").append(summary.getNumberOfRatings()).append("</td>")
                    .append("<td>").append(summary.getAvgGeneralSatisfaction()).append("</td>")
                    .append("<td>").append(summary.getAvgExpertiseKnowledge()).append("</td>")
                    .append("<td>").append(summary.getAvgAttitude()).append("</td>")
                    .append("<td>").append(summary.getAvgResponseSpeed()).append("</td>")
                    .append("<td>").append(summary.getAvgUnderstanding()).append("</td>")
                    .append("<td>").append(summary.getAvgOverallScore()).append("</td>")
                    .append("</tr>");
        }
        return dataRows.toString();
    }

    @Override
    public void generateAdvisorSummaryExcel(List<AdvisorSummaryDTO> summaries, HttpServletResponse response) throws IOException {
        List<String> headers = List.of(
                "Tên tư vấn viên",
                "Email",
                "Số lượng đánh giá",
                "Mức độ hài lòng chung",
                "Kiến thức chuyên môn",
                "Thái độ",
                "Tốc độ phản hồi",
                "Sự hiểu và chính xác",
                "Tổng số điểm"
        );

        List<List<String>> data = summaries.stream()
                .map(summary -> List.of(
                        summary.getConsultantName(),
                        summary.getConsultantEmail(),
                        String.valueOf(summary.getNumberOfRatings()),
                        String.valueOf(summary.getAvgGeneralSatisfaction()),
                        String.valueOf(summary.getAvgExpertiseKnowledge()),
                        String.valueOf(summary.getAvgAttitude()),
                        String.valueOf(summary.getAvgResponseSpeed()),
                        String.valueOf(summary.getAvgUnderstanding()),
                        String.valueOf(summary.getAvgOverallScore())
                ))
                .collect(Collectors.toList());

        excelService.generateExcelFile("Advisor Summary", headers, data, "advisor_summary_" + excelService.currentDate(), response);
    }
}
