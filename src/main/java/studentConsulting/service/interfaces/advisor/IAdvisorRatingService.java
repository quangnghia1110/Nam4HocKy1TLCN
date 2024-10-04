package studentConsulting.service.interfaces.advisor;

import com.lowagie.text.DocumentException;
import org.springframework.data.domain.Page;
import studentConsulting.model.payload.dto.rating.RatingDTO;
import studentConsulting.model.payload.dto.user.AdvisorSummaryDTO;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.time.LocalDate;
import java.util.List;

public interface IAdvisorRatingService {
    Page<RatingDTO> getRatingsByDepartment(Integer departmentId, String consultantName, LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir);

    RatingDTO getRatingByIdAndDepartment(Integer ratingId, Integer departmentId);

    List<AdvisorSummaryDTO> getAdvisorSummariesByDepartment(Integer departmentId);

    void generateAdvisorSummaryPdf(List<AdvisorSummaryDTO> summaries, HttpServletResponse response) throws DocumentException, IOException;

    void generateAdvisorSummaryExcel(List<AdvisorSummaryDTO> summaries, HttpServletResponse response) throws IOException;

}
